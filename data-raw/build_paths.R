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
library(lwgeom)

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

deployments_current <- read_feather("data-raw/deployments")

# need the lpo shapefile to be able to do a spatial filter; this version
# is cutting off some of the edges, so buffering, and to do that projecting
# to UTM (EPSG: 32611)

crs_project <- "EPSG:32611"

# set the resolution (in meters) that will
# be applied to the raster

res_m <- 25

lpo.sf_map <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "lakes"
) %>%
  filter(name == "Lake Pend Oreille")

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
