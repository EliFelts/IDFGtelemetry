######################################################
# Script for building networks of 0.1 km             #
# spaced points along least distance paths           #
# between receiver deployments; this script          #
# can be reused to add paths when new deployment     #
# locations are added                                #
######################################################

# load libraries

library(tidyverse)
library(conflicted)
library(sf)
library(leastcostpath)
library(gdistance)
library(terra)
library(units)
library(leaflet)
library(leafem)
library(here)
library(arrow)
library(rnaturalearth)
library(lwgeom)
library(readxl)

# declare conflict preferences


conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  base::unique,
  dplyr::lag,
  tidyr::expand
)

# build a base map in leaflet to be able to do some
# quick visual checks as code is worked out


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
leaflet_base

# pull in deployments

deployments_current <- read_excel("data-raw/Receiver Info 7_1_25.xlsx") |>
  filter(waterbody == "Lake Pend Oreille")

# need the lpo shapefile to be able to do a spatial filter; this version
# is cutting off some of the edges, so buffering, and to do that projecting
# to UTM (EPSG: 32611)

# crs_project <- "EPSG:32611"

# grab polygon of Idaho to filter
# clark for within the state

id.sf <- ne_states(returnclass = "sf") |>
  filter(name == "Idaho")

lpo.sf_map <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "lakes"
) %>%
  filter(name == "Lake Pend Oreille")


rivers.sf <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "streams"
) |>
  filter(name %in% c(
    "Clark Fork",
    "Lightning Creek",
    "Pack River"
  )) |>
  st_intersection(id.sf)


rivers_buffer.sf <- st_buffer(rivers.sf, dist = 50)

water_corridor <- c(st_geometry(lpo.sf_map), st_geometry(rivers_buffer.sf)) |>
  st_union() |>
  st_as_sf()

deploy_map1 <- deployments_current %>%
  filter(
    !is.na(Latitude),
    !is.na(Longitude),
    !is.na(`Location Name`)
  ) %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )

leaflet_base |>
  addPolygons(data = water_corridor) |>
  addCircleMarkers(
    data = deploy_map1,
    label = ~ str_c(`Location Name`)
  )


# project the water corridor to a projected
# CRS before doing raster processing

crs_m <- 26911

water_corridor.projected <- st_transform(water_corridor, crs_m) |>
  st_buffer(dist = 25) |>
  st_make_valid()

# need to do the same with receiver points, first
# filter some of those then project

deployments.projected <- deployments_current %>%
  filter(
    !is.na(Latitude),
    !is.na(Longitude),
    !is.na(`Location Name`)
  ) %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  ) %>%
  st_transform(crs_m)

# build a raster template

# choose cell size (25 m for now)

cell <- 25

r_template <- rast(vect(water_corridor.projected), resolution = cell)

# put a 5 cell buffer around the template to help with edge effects

r_template <- extend(r_template, 5)

# rasterize the corridor into a mask, basically
# make water the only area where "cost" makes sense
# to travel

water_v <- vect(water_corridor.projected)

r_water <- rasterize(water_v, r_template, field = 1, background = NA)

r_water <- ifel(is.na(r_water), NA, 1)

# snap deployments to the corridor - they must start on
# water cells

deployments_snap <- st_snap(deployments.projected, water_corridor.projected, tolerance = 100)

# snap to nearest water cell center (raster-level)

rcells <- cellFromXY(r_water, sf::st_coordinates(deployments_snap))

# visual check

leaflet_base |>
  addPolygons(data = st_transform(water_corridor.projected, 4326)) |>
  addCircleMarkers(
    data = st_transform(deployments_snap, 4326),
    popup = ~ str_c(`Location Name`)
  )


# make r_water a transition RasterLayer

r_cost_raster <- raster::raster(r_water)

tr <- gdistance::transition(r_cost_raster, transitionFunction = function(x) 1 / mean(x), directions = 8)
tr <- gdistance::geoCorrection(tr, type = "c")

# get points prepped

xy <- st_coordinates(deployments_snap)

pts <- deployments_snap |>
  mutate(receiver_id = `Location Name`)

ids <- pts$receiver_id

# define which pairs to get lines for (all pairs)

pairs <- t(combn(ids, 2)) |>
  as.data.frame() |>
  setNames(c("from", "to"))

# comput LCP for each pair

get_lcp <- function(id1, id2) {
  p1 <- xy[pts$receiver_id == id1, , drop = FALSE]
  p2 <- xy[pts$receiver_id == id2, , drop = FALSE]

  sl <- gdistance::shortestPath(tr, p1, p2, output = "SpatialLines")
  sf::st_as_sf(sl) |>
    dplyr::mutate(from = id1, to = id2)
}

paths_sf <- pmap_dfr(pairs, get_lcp)

# pull out function guts and test

p1.test <- xy[pts$receiver_id == "CF Above Hatchery", , drop = FALSE]
p2.test <- xy[pts$receiver_id == "Sheepherder Point", , drop = FALSE]

sl.test <- shortestPath(tr, p1.test, p2.test, output = "SpatialLines")

output.test <- sf::st_as_sf(sl.test) |>
  mutate(
    from = "CF Above Hatchery",
    to = "Sheepherder Point"
  )

##### try to adapt previous working code

water_corridor_wgs <- water_corridor.projected |>
  st_transform(crs = 4326)

lpo.raster <- ext(water_corridor_wgs)

# and then each cell in the grid will have a value representing a "cost".
# We're starting with a cost of 1 for each box here

lpo.raster_grid <- rast(lpo.raster,
  nrow = 1000, ncol = 1000,
  crs = crs(water_corridor_wgs),
  vals = 1
)


# Now we're going to differentiate the reservoir within the raster rectangle

lpo.cost <- mask(lpo.raster_grid,
  vect(water_corridor_wgs),
  updatevalue = NA
)

# define lcp function

lcp_build.f <- function(points.df,
                        cost.raster = lpo.cost) {
  start_point <- points.df %>%
    st_as_sf(
      coords = c("start_long", "start_lat"),
      crs = 4326
    )

  end_point <- points.df %>%
    st_as_sf(
      coords = c("end_long", "end_lat"),
      crs = 4326
    )

  end.location <- cellFromXY(
    cost.raster,
    st_coordinates(end_point)
  )

  start.location <- cellFromXY(
    cost.raster,
    st_coordinates(start_point)
  )

  values(cost.raster)[start.location] <- 2

  cost.dist <- gridDist(cost.raster, target = 2)

  start.sp <- start_point %>%
    as_Spatial()

  end.sp <- end_point %>%
    as_Spatial()

  cost.dist.raster <- raster(cost.dist)

  transition_surface <- create_distance_cs(cost.dist.raster, neighbours = 16)

  least_cost_path <- create_lcp(
    transition_surface, start.sp,
    end.sp
  )

  lcp_sf <- st_as_sf(least_cost_path) %>%
    filter(direction == "A to B") %>%
    mutate(
      start = points.df$start,
      end = points.df$end
    )
}


# run the function to create paths;
# doing one starting location at a time
# because this takes a long time and if
# it fails don't want to have to redo the entire
# thing

# make pairs of coords to plug into the function

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
  filter(!location_name == "Mouth of Lightning Cr in North Sidechannel")

pairs.df <- receiver.dat |>
  select(
    location_name, longitude,
    latitude
  ) |>
  expand(location_name, location_name) |>
  select(start = 1, end = 2) |>
  filter(!start == end)

pairs_coords <- pairs.df |>
  left_join(receiver.dat, by = c("start" = "location_name")) |>
  select(start, end, start_long = longitude, start_lat = latitude) %>%
  left_join(receiver.dat, by = c("end" = "location_name")) %>%
  select(start, end, start_long, start_lat,
    end_long = longitude, end_lat = latitude
  ) %>%
  rownames_to_column()


# Anderson Point

anderson <- pairs_coords %>%
  filter(start == "Anderson Point")

anderson_list <- split(anderson, f = anderson$rowname)

anderson_paths <- bind_rows(map(anderson_list, lcp_build.f))


# pull out one line to debug

test_startpoint <- anderson_list$`100` |>
  st_as_sf(
    coords = c("start_long", "start_lat"),
    crs = 4326
  )

test_endpoint <- anderson_list$`100` |>
  st_as_sf(
    coords = c("end_long", "end_lat"),
    crs = 4326
  )

test_end.location <- cellFromXY(
  lpo.cost,
  st_coordinates(test_endpoint)
)

test_start.location <- cellFromXY(
  lpo.cost,
  st_coordinates(test_startpoint)
)

terra::values(lpo.cost)[test_start.location] <- 2

test_cost.dist <- gridDist(lpo.cost, target = 2)

test_start.sp <- test_startpoint |>
  as_Spatial()

test_end.sp <- test_endpoint |>
  as_Spatial()

test_cost.dist.raster <- raster(lpo.cost)

transition_surface <- create_distance_cs(test_cost.dist.raster, neighbours = 16)


############################ names_inform_repair()

lakes.sf <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "lakes"
)

lpo.sf <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "lakes"
) %>%
  filter(name == "Lake Pend Oreille") %>%
  st_make_valid() %>%
  st_transform(crs_project)

# make a slightly buffered shape for the lake
# so nearshore points aren't excluded

lpo_buffered.sf <- lpo.sf %>%
  sf::st_buffer(50)

# get point pairs to get paths between

lake_deployments <- deployments_current %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(location_name)
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>%
  st_transform(crs_project) %>%
  st_filter(lpo_buffered.sf, .predicate = st_within) %>%
  arrange(location_name) %>%
  distinct(location_name, .keep_all = T)

# get all pairs of deployments

locs <- unique(lake_deployments$location_name)

lakepairs.df <- tidyr::expand_grid(
  from_id = locs, to_id = locs
) %>%
  filter(from_id != to_id)







# get pairs of coordinates for those

lakepairs_coords <- lakepairs.df %>%
  left_join(lake_deployments, by = c("from" = "location_name")) %>%
  rename(start_long = longitude, start_lat = latitude) %>%
  left_join(lake_deployments, by = c("to" = "location_name")) %>%
  select(from, to, start_long, start_lat,
    end_long = longitude, end_lat = latitude
  ) %>%
  rownames_to_column()






deploy_map1 <- deployments_current %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(location_name)
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )

deploy_map <- lake_deployments |>
  st_transform(crs = 4326)

leaflet_base |>
  addPolygons(data = lpo.sf_map) |>
  addCircleMarkers(
    data = deploy_map1,
    fillColor = "magenta",
    color = "magenta"
  ) |>
  addCircleMarkers(
    data = deploy_map,
    popup = ~location_name
  )

leaflet_base |>
  addPolygons(data = lakes.sf)
