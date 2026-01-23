library(arrow)
library(tidyverse)
library(usethis)


detections <- read_feather("data-raw/fishdetections_jan26")

detections_ex <- detections %>%
  filter(species == "WAE") %>%
  mutate(detection_year = year(detection_date)) %>%
  filter(
    detection_year == 2024,
    flag_false == F
  )

detection.sum <- detections_ex %>%
  group_by(fish_id) %>%
  summarize(
    locations = n_distinct(location_id),
    detections = n()
  ) %>%
  filter(locations > 15)

detections_example <- detections_ex %>%
  filter(fish_id %in% detection.sum$fish_id |
    fish_id == "1327666_2020-04-28_WAE")

use_data(detections_example, overwrite = TRUE)
