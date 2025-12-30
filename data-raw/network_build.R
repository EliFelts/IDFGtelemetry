library(readr)
library(usethis)

network_points <- read_csv("data-raw/complete_point_network.csv")

use_data(network_points, overwrite = TRUE)
