library(readr)
library(arrow)
library(usethis)
library(tidyverse)
library(sf)
library(readxl)

# define the path to where shared files are stored (will change
# to Sharepoint once we get that setup)

shared_parent.dir <- "~/Library/CloudStorage/OneDrive-SunnysideInsights/LPO_Acoustic_Telemetry_Sync"

# Read in deployment locations

locations_current <- read_excel(path = str_c(shared_parent.dir, "deployment_locations.xlsx", sep = "/")) |>
  filter(waterbody == "Lake Pend Oreille")

use_data(locations_current, overwrite = TRUE)

## make map for animations

lpo_lines <- st_read(
  dsn = "data-raw/lpo_streams.gpkg",
  layer = "map_streams"
)

use_data(lpo_lines, overwrite = T)

lpo_polygon <- st_read(
  dsn = "data-raw/idfg_habitat_mapping.gpkg",
  layer = "lakes"
)

use_data(lpo_polygon, overwrite = T)
