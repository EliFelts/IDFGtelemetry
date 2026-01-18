library(conflicted)
library(tidyverse)
library(readxl)
library(sf)
library(terra)
library(gdistance)
library(leaflet)
library(leafem)
library(rnaturalearth)
library(units)

conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  base::unique,
  dplyr::lag,
  tidyr::expand
)

leaflet_base <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Roads") %>%
  addLayersControl(
    baseGroups = c("Topographic", "Imagery", "Roads"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  ) %>%
  setView(lng = -116.53906, lat = 48.1, zoom = 10) %>%
  addMouseCoordinates()

# Read in current deployment data, filter for LPO system
# and drop any that don't have coordinates


receiver.dat <- read_excel("data-raw/Receiver Info 7_1_25.xlsx") |>
  filter(
    waterbody == "Lake Pend Oreille",
    !is.na(Latitude),
    !is.na(Longitude),
    !is.na(`Location Name`)
  ) |>
  select(
    location_name = `Location Name`,
    longitude = Longitude,
    latitude = Latitude
  ) |>
  distinct() |>
  filter(!location_name %in% c(
    "Cement Plant Replacement",
    "Pack Delta RR Replacement",
    "Riley Creek Replacement"
  ))

# set meter-base CRS to put all pieces
# into for raster process

target_crs <- 32611

# bring in lake polygon and also
# convert to the same CRS

lake <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "lakes"
) %>%
  filter(name == "Lake Pend Oreille") |>
  st_transform(target_crs)

# streams added too

id.sf <- ne_states(returnclass = "sf") |>
  filter(name == "Idaho")


streams <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "streams"
) |>
  filter(name %in% c(
    "Clark Fork",
    "Lightning Creek",
    "Pack River"
  )) |>
  st_intersection(id.sf) |>
  st_transform(target_crs) |>
  st_make_valid()

# union the lake with a very small buffer of streams to force contact

buf_m <- 80
snap_buf_m <- 20

lake_bridged <- st_union(lake, st_buffer(streams, snap_buf_m))
lake_bridged <- st_make_valid(lake_bridged)

stream_poly <- st_buffer(streams, buf_m) |> st_make_valid()

water_surface <- st_union(lake_bridged, stream_poly) |>
  st_make_valid() |>
  st_cast("MULTIPOLYGON")

lake_center <- st_point_on_surface(lake) |> st_geometry()

parts <- st_cast(water_surface, "POLYGON")
idx <- st_within(lake_center, parts, sparse = TRUE)[[1]]

water_surface_main <- st_as_sf(parts[idx])

water_surface_main <- water_surface_main |>
  st_union() |>
  st_as_sf()


clean_m <- 2
water_surface_main <- water_surface_main |>
  st_buffer(clean_m) |>
  st_buffer(-clean_m) |>
  st_make_valid()

# convert receiver points to target CRS,
# and work through process of making sure
# all are within the water surface raster layer

target_crs2 <- st_crs(water_surface_main)

deployments.example <- receiver.dat |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) |>
  st_transform(target_crs2) |>
  st_make_valid()

# find which points are inside vs. outside the current water surface
# boundaries

inside <- st_within(deployments.example, water_surface_main, sparse = F)[, 1]
table(inside)

deployments.example_in <- deployments.example[inside, ]

deployments.example_out <- deployments.example[!inside, ]

# For each outside point, get a LINESTRING to nearest point on polygon

nearest_lines <- st_nearest_points(deployments.example_out, water_surface_main)

# Extract the second coordinate (the point on the polygon)
snap_coords <- lapply(nearest_lines, function(g) {
  xy <- st_coordinates(g)
  xy[nrow(xy), 1:2, drop = FALSE] # last vertex is on polygon
})
snap_coords <- do.call(rbind, snap_coords)

deployments.example_out_snapped <- deployments.example_out
st_geometry(deployments.example_out_snapped) <- st_sfc(lapply(seq_len(nrow(snap_coords)), function(i) {
  st_point(snap_coords[i, ])
}), crs = st_crs(deployments.example_out))

deployments.example_snapped <- deployments.example_out_snapped |>
  bind_rows(deployments.example_in)

snap_to_valid_cell <- function(pts_sf, lake_raster, max_dist_m = Inf) {
  stopifnot(inherits(pts_sf, "sf"))

  # terra wants matrix of xy in raster CRS
  xy <- sf::st_coordinates(pts_sf)

  # cells that are currently NA
  cells <- terra::cellFromXY(lake_raster, xy)

  need <- which(is.na(cells))
  if (length(need) == 0) {
    return(pts_sf)
  }

  # all valid (non-NA) cells in the raster
  valid_cells <- which(!is.na(terra::values(lake_raster, mat = FALSE)))
  valid_xy <- terra::xyFromCell(lake_raster, valid_cells)

  # for each NA point, find nearest valid cell center
  for (i in need) {
    dx <- valid_xy[, 1] - xy[i, 1]
    dy <- valid_xy[, 2] - xy[i, 2]
    j <- which.min(dx * dx + dy * dy)

    # optional: reject if too far
    if (is.finite(max_dist_m)) {
      if (sqrt(dx[j]^2 + dy[j]^2) > max_dist_m) next
    }

    xy[i, ] <- valid_xy[j, ]
  }

  sf::st_geometry(pts_sf) <- sf::st_sfc(lapply(seq_len(nrow(xy)), function(i) sf::st_point(xy[i, ])), crs = sf::st_crs(pts_sf))
  pts_sf
}

# Build the lake cost-distance raster

# Make a raster grid over the lake extent

res_m <- 25

r_template <- rast(ext(vect(water_surface_main)),
  resolution = res_m,
  crs = paste0("EPSG:", target_crs)
)

# rasterize: inside lake = 1, outside = NA

lake_r <- rasterize(vect(water_surface_main), r_template, field = 1, background = NA)

# apply after your polygon snap:
deployments.example_snapped2 <- snap_to_valid_cell(deployments.example_snapped, lake_r)

## trying to fix points still not getting raster value of 1

r_template <- lake_r

# Rasterize polygon so *touched* cells are included
water_mask <- terra::rasterize(
  terra::vect(water_surface_main),
  r_template,
  field = 1,
  touches = TRUE,
  background = NA
)

# (Optional) convert to a cost raster: water = 1, land = NA
cost_r <- water_mask
terra::values(cost_r)[terra::values(cost_r) == 1] <- 1



# make sure the points fall within a valid cell of lake_r

snap_to_valid_cell <- function(pts_sf, lake_raster, max_dist_m = Inf) {
  stopifnot(inherits(pts_sf, "sf"))

  # terra wants matrix of xy in raster CRS
  xy <- sf::st_coordinates(pts_sf)

  # cells that are currently NA
  cells <- terra::cellFromXY(lake_raster, xy)

  need <- which(is.na(cells))
  if (length(need) == 0) {
    return(pts_sf)
  }

  # all valid (non-NA) cells in the raster
  valid_cells <- which(!is.na(terra::values(lake_raster, mat = FALSE)))
  valid_xy <- terra::xyFromCell(lake_raster, valid_cells)

  # for each NA point, find nearest valid cell center
  for (i in need) {
    dx <- valid_xy[, 1] - xy[i, 1]
    dy <- valid_xy[, 2] - xy[i, 2]
    j <- which.min(dx * dx + dy * dy)

    # optional: reject if too far
    if (is.finite(max_dist_m)) {
      if (sqrt(dx[j]^2 + dy[j]^2) > max_dist_m) next
    }

    xy[i, ] <- valid_xy[j, ]
  }

  sf::st_geometry(pts_sf) <- sf::st_sfc(lapply(seq_len(nrow(xy)), function(i) sf::st_point(xy[i, ])), crs = sf::st_crs(pts_sf))
  pts_sf
}

# apply after your polygon snap:
deployments.example_snapped2 <- snap_to_valid_cell(deployments.example_snapped, lake_r)


# ---- 3) cost surface + transition graph ----
# We want movement allowed only through non-NA cells.
# transitionFunction below creates a conductance graph (higher = easier).

tr <- transition(raster::raster(cost_r), transitionFunction = function(x) 1, directions = 8)
tr <- geoCorrection(tr, type = "c", scl = FALSE)

# now define a function that will use the inputs of the
# points that are within the raster, the lake raster,
# and the transition surface to build the network of
# least cost paths between deployment locations


build_paths <- function(start, end,
                        snapped_deployments = deployments.example_snapped2,
                        lake_raster = lake_r,
                        transition_layer = tr) {
  p1 <- snapped_deployments |>
    dplyr::filter(location_name == start)

  p2 <- snapped_deployments |>
    dplyr::filter(location_name == end)

  xy1 <- sf::st_coordinates(p1)
  xy2 <- sf::st_coordinates(p2)

  cell1 <- terra::cellFromXY(lake_raster, xy1)
  cell2 <- terra::cellFromXY(lake_raster, xy2)

  if (is.na(cell1) || is.na(cell2)) {
    stop("One of the points is not on a valid (inside-lake) cell at this raster resolution.")
  }

  start_xy <- terra::xyFromCell(lake_raster, cell1)
  end_xy <- terra::xyFromCell(lake_raster, cell2)

  sp <- gdistance::shortestPath(transition_layer,
    origin = start_xy,
    goal = end_xy,
    output = "SpatialLines"
  )

  path_sf <- sf::st_as_sf(sp)

  spacing_m <- 100

  len <- as.numeric(sf::st_length(path_sf))

  d <- seq(0, len, by = spacing_m)

  if (tail(d, 1) < len) d <- c(d, len) # ensure endpoint included

  sfc_line <- sf::st_geometry(path_sf)[[1]]
  pts_sfc <- sf::st_line_sample(sfc_line, sample = d / len, type = "regular")

  path_pts_sf <- sf::st_as_sf(pts_sfc) |>
    sf::st_cast("POINT")
  path_pts_sf$dist_m <- d

  sf::st_crs(path_pts_sf) <- sf::st_crs(water_surface_main)

  path_pts_4326 <- sf::st_transform(path_pts_sf, 4326) |>
    dplyr::mutate(name = stringr::str_c(start, "to", end, sep = "_"))

  return(path_pts_4326)
}


test1 <- build_paths(
  start = "Farragut Breakwater",
  end = "River Right Railroad"
)

# loop through all possibilities with Farragut Breakwater as the start

leaflet_base |>
  addPolygons(data = st_transform(water_surface_main, crs = 4326)) |>
  addCircleMarkers(
    data = st_transform(deployments.example_snapped2, crs = 4326),
    label = ~ str_c(location_name)
  ) |>
  addCircleMarkers(data = st_transform(test1, crs = 4326))

start_id <- "Farragut Breakwater"

id_col <- "location_name"

start_pt <- deployments.example_snapped |>
  filter(.data[[id_col]] == start_id)

end_pts <- deployments.example_snapped %>%
  filter(.data[[id_col]] != start_id)

library(tictoc)
tic()
farragut_test <- map(
  end_pts$location_name,
  ~ build_paths(start = start_id, end = .x)
) |>
  bind_rows()
toc()

# test1 <- farragut_test |>
#   filter(name == "Farragut Breakwater_to_CF at Twin Creek Mouth")
#
# leaflet_base |>
#   addPolygons(data = st_transform(water_surface_main, crs = 4326)) |>
#   addCircleMarkers(data = st_transform(test1, crs = 4326))

# create df of all possible combinations

pairs.df <- deployments.example_snapped2 %>%
  st_drop_geometry() |>
  expand(location_name, location_name) %>%
  select(start = 1, end = 2) %>%
  filter(!start == end)

n_locs <- deployments.example_snapped2 |>
  st_drop_geometry() |>
  distinct(location_name) |>
  nrow()

n_pairs <- n_locs * (n_locs - 1)

n_locs
n_pairs


log_file <- "path_log.csv"
if (!file.exists(log_file)) writeLines("i,start,end,stage,message", log_file)

out_gpkg <- "all_paths.gpkg"
layer <- "paths_pts"
if (!file.exists(out_gpkg)) {
  # create file on first successful write
}

n <- nrow(pairs.df)

tic()
for (i in seq_len(n)) {
  st <- pairs.df$start[i]
  en <- pairs.df$end[i]

  # log BEFORE attempt
  cat(i, st, en, "START", "", sep = ",", file = log_file, append = TRUE)
  cat("\n", file = log_file, append = TRUE)

  res <- tryCatch(
    build_paths(
      start = st, end = en,
      snapped_deployments = deployments.example_snapped2,
      lake_raster = cost_r,
      transition_layer = tr
    ),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    cat(i, st, en, "ERR", gsub(",", ";", res$message), sep = ",", file = log_file, append = TRUE)
    cat("\n", file = log_file, append = TRUE)
  } else {
    sf::st_write(res, out_gpkg, layer = layer, append = file.exists(out_gpkg), quiet = TRUE)
    cat(i, st, en, "OK", "", sep = ",", file = log_file, append = TRUE)
    cat("\n", file = log_file, append = TRUE)
  }

  rm(res)
  if (i %% 25 == 0) gc()
}
toc()
