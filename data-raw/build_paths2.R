library(conflicted)
library(tidyverse)
library(readxl)
library(sf)
library(terra)
library(gdistance)
library(leaflet)
library(leafem)
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

# convert receiver points to target CRS

deployments.example <- receiver.dat |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) |>
  st_transform(target_crs)

# pull out one simple pair to work
# out an example, McDonald's Dock to
# Farragut Breakwater

p1 <- deployments.example |>
  filter(location_name == "McDonald's Dock")

p2 <- deployments.example |>
  filter(location_name == "Farragut Breakwater")

# bring in lake polygon and also
# convert to the same CRS

lake <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "lakes"
) %>%
  filter(name == "Lake Pend Oreille") |>
  st_transform(target_crs)


# Map that simple pair and lake polygon just to visualize
# what's going on

leaflet_base |>
  addPolygons(data = st_transform(lake, crs = 4326)) |>
  addCircleMarkers(
    data = st_transform(p1, 4326)
  ) |>
  addCircleMarkers(
    data = st_transform(p2, 4326)
  )

# Make a raster grid over the lake extent

res_m <- 100

r_template <- rast(ext(vect(lake)),
  resolution = res_m,
  crs = paste0("EPSG:", target_crs)
)

# rasterize: inside lake = 1, outside = NA

lake_r <- rasterize(vect(lake), r_template, field = 1, background = NA)


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

st_crs(path_pts_sf) <- st_crs(lake)

path_pts_4326 <- st_transform(path_pts_sf, 4326)










plot(st_geometry(lake), col = "lightblue", border = "grey40")
plot(st_geometry(path_sf), add = TRUE, lwd = 2, col = "red")
plot(st_geometry(p1), add = TRUE, pch = 16)
plot(st_geometry(p2), add = TRUE, pch = 16)

leaflet_base |>
  addPolygons(data = st_transform(lake, crs = 4326)) |>
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
