#' Interpolate hourly detections of individual fish based on observations
#'
#' Given a data frame of detections, interpolates fish positions at each hour,
#' using spatial paths between static receiver deployment locations.
#'
#' @param detections Data frame with columns: fish_id (character), detection_datetime (POSIXct),
#'   latitude (numeric), longitude (numeric), location_name (character).
#' @param fish_id Character scalar of vector; unique fish identifier(s), e.g. `"1302740_2018-10-10_LKT"`
#' @param paths Data frame of points (lat/long in WGS 84) between pairwise
#' combinations of receiver deployments; defaults to network_points which loads
#' the most recent version in the IDFGtelemetry package
#' @param deployments Data frame of points (lat/long in WGS 84) for receiver deployments;
#' default is deployments_current which loads the most recent version in the
#' IDFGtelemetry package
#' @return Tibble with hourly positions ('det_lat','det_long') for each fish_id, with
#'  fields indicating observed vs interpolated, and associated receiver/location name
#' @export
#' @examples
#' data(detection_examples)
#' out <- interpolate_hourly(detections_example, fish_id = "1327672_2019-11-06_WAE")
#' head(out)
interpolate_hourly <- function(detections, fish_id, paths = network_points, deployments = deployments_current) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr needed for this function.")
  required_det_cols <- c("fish_id", "latitude", "longitude", "detection_datetime", "location_name")
  missing_cols <- setdiff(required_det_cols, colnames(detections))
  if (length(missing_cols) > 0) stop("detections is missing columns: ", paste(missing_cols, collapse = ", "))

  dat1 <- detections |>
    dplyr::filter(
      .data$fish_id %in% .env$fish_id,
      !is.na(.data$latitude),
      !is.na(.data$location_name)
    ) |>
    dplyr::arrange(.data$detection_datetime) |>
    dplyr::mutate(detection_hour = lubridate::round_date(.data$detection_datetime, unit = "hour"))

  ind_timeframe <- tibble::tibble(detection_hour = seq(min(dat1$detection_hour),
    max(dat1$detection_hour),
    by = "hours"
  ))

  pts1 <- dat1 |>
    dplyr::group_by(detection_hour) |>
    dplyr::summarize(
      det_lat = dplyr::first(.data$latitude),
      det_long = dplyr::first(.data$longitude),
      location_name = dplyr::first(.data$location_name),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prev_location = dplyr::lag(.data$location_name),
      next_location = dplyr::lead(.data$location_name),
      next_detection_hr = dplyr::lead(.data$detection_hour)
    )

  path_attributes <- paths |>
    dplyr::group_by(name) |>
    dplyr::summarize(length = max(rkm))

  join1 <- ind_timeframe |>
    dplyr::left_join(pts1, by = "detection_hour") |>
    dplyr::mutate(
      transition_start = ifelse(.data$location_name == .data$next_location,
        FALSE, TRUE
      ),
      transition_name = stringr::str_c(.data$location_name, "to", .data$next_location,
        sep = "_"
      )
    ) |>
    tidyr::fill(.data$transition_start, .direction = "down") |>
    dplyr::mutate(transition_start = dplyr::coalesce(.data$transition_start, FALSE)) |>
    dplyr::left_join(path_attributes, by = c("transition_name" = "name"))

  transition <- join1 |>
    dplyr::filter(transition_start == TRUE) |>
    dplyr::mutate(
      interval_id = stringr::str_c(detection_hour, transition_name, sep = "_"),
      transition_distance = length
    ) |>
    tidyr::fill(interval_id, .direction = "down") |>
    tidyr::fill(next_detection_hr, .direction = "down") |>
    dplyr::group_by(interval_id) |>
    dplyr::mutate(
      group_count = dplyr::n(),
      group_start_hour = dplyr::first(detection_hour),
      rowname = dplyr::row_number(),
      advance_rate = transition_distance / group_count,
      join_name = transition_name
    ) |>
    tidyr::fill(advance_rate, .direction = "down") |>
    tidyr::fill(join_name, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      calculated_rkm = round(((rowname - 1) * advance_rate), 1),
      point_type = ifelse(is.na(det_lat), "interpolated", "observed")
    ) |>
    dplyr::select(
      detection_hour, join_name, det_lat, det_long,
      point_type, calculated_rkm
    ) |>
    dplyr::mutate(calculated_rkm = ifelse(calculated_rkm == 0, 0.1,
      calculated_rkm
    )) |>
    dplyr::left_join(paths, by = c(
      "join_name" = "name",
      "calculated_rkm" = "rkm"
    )) |>
    dplyr::mutate(
      det_lat = ifelse(point_type == "interpolated", lat, det_lat),
      det_long = ifelse(point_type == "interpolated", lon, det_long),
      det_rkm = calculated_rkm
    )

  static <- join1 |>
    dplyr::filter(transition_start == FALSE) |>
    tidyr::fill(location_name, .direction = "down") |>
    dplyr::left_join(deployments, by = c("location_name")) |>
    dplyr::mutate(
      join_name = stringr::str_c(location_name, location_name, sep = "_"),
      det_rkm = 0,
      point_type = ifelse(is.na(det_lat), "interpolated", "observed"),
      calculated_rkm = 0
    ) |>
    dplyr::select(detection_hour, join_name,
      det_lat = latitude, det_long = longitude,
      det_rkm, point_type, calculated_rkm
    )

  hourly_bind <- transition |>
    dplyr::select(-c(rowname, lon, lat)) |>
    dplyr::bind_rows(static) |>
    dplyr::mutate(fish_id = .env$fish_id)
}
