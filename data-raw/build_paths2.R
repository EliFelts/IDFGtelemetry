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

# starting out with just lpo_lake receivers bc the
# lines will be constructed a little bit differently
# for movement between those receivers

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
  distinct()

# work out an example first

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

# now bring in deployment points and
# make sure they're in the same projection as
# the water surface and also
# that they fall within that layer

# convert receiver points to target CRS

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


leaflet_base |>
  addPolygons(data = st_transform(water_surface_main, crs = 4326)) |>
  addCircleMarkers(
    data = st_transform(deployments.example_out_snapped, crs = 4326),
    label = ~ str_c(location_name)
  )

# pull out one simple pair to work
# out an example, McDonald's Dock to
# Farragut Breakwater

p1 <- deployments.example_snapped |>
  filter(location_name == "Farragut Breakwater")

p2 <- deployments.example_snapped |>
  filter(location_name == "CF River near RR Bridge")





# Map that simple pair and lake polygon just to visualize
# what's going on

leaflet_base |>
  addPolygons(data = st_transform(water_surface_main, crs = 4326)) |>
  addCircleMarkers(
    data = st_transform(p1, 4326)
  ) |>
  addCircleMarkers(
    data = st_transform(p2, 4326)
  )

# Make a raster grid over the lake extent

res_m <- 50

r_template <- rast(ext(vect(water_surface_main)),
  resolution = res_m,
  crs = paste0("EPSG:", target_crs)
)

# rasterize: inside lake = 1, outside = NA

lake_r <- rasterize(vect(water_surface_main), r_template, field = 1, background = NA)


# ---- 3) cost surface + transition graph ----
# We want movement allowed only through non-NA cells.
# transitionFunction below creates a conductance graph (higher = easier).

tr <- transition(raster::raster(lake_r), transitionFunction = function(x) 1, directions = 8)
tr <- geoCorrection(tr, type = "c", scl = FALSE)

# ---- 4) snap points to valid cells (inside lake) ----
# terra::cellFromXY returns NA if point falls in NA (outside lake or in a hole).
xy1 <- st_coordinates(p1)
xy2 <- st_coordinates(p2)

cell1 <- cellFromXY(lake_r, xy1)
cell2 <- cellFromXY(lake_r, xy2)


if (is.na(cell1) || is.na(cell2)) {
  stop("One of the points is not on a valid (inside-lake) cell at this raster resolution.")
}

# gdistance wants coordinates; safest is to use the cell centers you snapped to:
start_xy <- xyFromCell(lake_r, cell1)
end_xy <- xyFromCell(lake_r, cell2)

### check to diagnose CF stuff

plot(lake_r, colNA = "white")
plot(vect(water_surface_main), add = TRUE, border = "blue", lwd = 2)
points(start_xy[1], start_xy[2], pch = 16)
points(end_xy[1], end_xy[2], pch = 16)



# ---- 5) shortest path constrained to lake cells ----
sp <- shortestPath(tr,
  origin = start_xy,
  goal   = end_xy,
  output = "SpatialLines"
)

# convert to sf
path_sf <- st_as_sf(sp)

spacing_m <- 100 # 0.1 km

# make sure the line is in a projected CRS with meters (you already did this)
len <- as.numeric(st_length(path_sf)) # meters

d <- seq(0, len, by = spacing_m)
if (tail(d, 1) < len) d <- c(d, len) # ensure endpoint included

# st_line_sample uses fractions of total length
sfc_line <- st_geometry(path_sf)[[1]]
pts_sfc <- st_line_sample(sfc_line, sample = d / len, type = "regular")

path_pts_sf <- st_as_sf(pts_sfc) |>
  st_cast("POINT")
path_pts_sf$dist_m <- d

st_crs(path_pts_sf) <- st_crs(water_surface_main)

path_pts_4326 <- st_transform(path_pts_sf, 4326)










plot(st_geometry(lake), col = "lightblue", border = "grey40")
plot(st_geometry(path_sf), add = TRUE, lwd = 2, col = "red")
plot(st_geometry(p1), add = TRUE, pch = 16)
plot(st_geometry(p2), add = TRUE, pch = 16)

leaflet_base |>
  addPolygons(data = st_transform(water_surface_main, crs = 4326)) |>
  addCircleMarkers(
    data = st_transform(p1, 4326)
  ) |>
  addCircleMarkers(
    data = st_transform(p2, 4326)
  ) |>
  addCircleMarkers(
    data = path_pts_4326,
    radius = 1
  )

leaflet_base |>
  addPolygons(data = st_transform(water_surface_main, crs = 4326)) |>
  addCircleMarkers(
    data = st_transform(deployments.example, crs = 4326),
    label = ~ str_c(location_name)
  )
