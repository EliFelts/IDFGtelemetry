library(arrow)
library(tidyverse)
library(usethis)


detections <- read_feather("data-raw/detections_nov25")

detections_example <- detections %>%
  filter(species == "WAE") %>%
  mutate(detection_year = year(detection_date)) %>%
  filter(detection_year == 2024)

detection.sum <- detections_example %>%
  group_by(fish_id) %>%
  summarize(locations = n_distinct(location_name)) %>%
  filter(locations > 15)

detections.export <- detections_example %>%
  filter(fish_id %in% detection.sum$fish_id)

use_data(detections.export, overwrite = TRUE)
