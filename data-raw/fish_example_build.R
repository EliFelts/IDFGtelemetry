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

add_fish <- c(
  "1351455_2021-04-19_WAE",
  "1327666_2020-04-28_WAE"
)

detections_example <- detections_ex %>%
  filter(fish_id %in% detection.sum$fish_id |
    fish_id %in% add_fish)

use_data(detections_example, overwrite = TRUE)
