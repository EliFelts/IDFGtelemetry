library(readr)
library(arrow)
library(usethis)
library(sf)
library(readxl)

network_points <- st_read("data-raw/all_paths.gpkg",
  layer = "paths_pts"
) |>
  mutate(rkm = dist_m / 1000)

# locations_current <- read_feather("data-raw/locations")
#
# use_data(locations_current, overwrite = T)

# network_points <- read_csv("data-raw/complete_point_network.csv")

use_data(network_points, overwrite = TRUE)

# define the path to where shared files are stored (will change
# to Sharepoint once we get that setup)

shared_parent.dir <- "~/Library/CloudStorage/OneDrive-SunnysideInsights/LPO_Acoustic_Telemetry_Sync"

# Read in deployment locations

locations_current <- read_excel(path = str_c(shared_parent.dir, "deployment_locations.xlsx", sep = "/")) |>
  filter(waterbody == "Lake Pend Oreille")

use_data(locations_current, overwrite = TRUE)
