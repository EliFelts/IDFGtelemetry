#' Identify clusters of telemetry detections that may indicate mortality or shedding events
#'
#' Given a data frame of detections, identifies clusters where individuals are detected at a single location
#' in a way that indicates the fish may have died or shed their tag within range of that receiver. Flagged
#' clusters are dropped if credible patterns of detections occur subsequently at different locations; "credible"
#' detections are those where individuals are present in at least three ten-minute bins at a different location
#' within a rolling 1-hour window. This criteria is intended to distinguish true subsequent presence from isolated
#' detection that may occur from long-range hits or noise. Flagged clusters should not be right-censored without
#' further inspection.
#'
#'
#' @param detection.df Data frame; must have columns: flag_false (logical), detection_datetime (POSIXct), fishi_id (character), location_id (character)
#' @param bin_duration Duration (in minutes) of temporal bins used to aggregate detections prior to identifying clusters. Detections occurring
#' within the same bin are treated as a single presence event. Default is 10 minutes
#' @param cluster_threshold Duration (in hours) over which sustained presence within bins (duration specified with bin_duration) must be observed for a
#' cluster to be flagged. Default is 24 hours and is intended to identify extended stationary periods.
#'
#' @returns Data frame with individual fish (fish_id) and location (location_id) that had at least one flagged cluster along with summaries
#' of the number of flagged clusters (flagged_runs), when the flagged runs started (flagged_runs_start), ended (flagged_runs_end) and
#' when the most recent flagged run started (latest_flagged_start)
#'
#' @export
#'
#' @examples
#' data("detections_example", package = "IDFGtelemetry")
#' out <- identify_cluster(detection.df = detections_example)
#' head(out)
identify_cluster <- function(detection.df,
                             bin_duration = 10,
                             cluster_threshold = 24) {
  bin_mins <- bin_duration

  presence_bins <- detection.df |>
    dplyr::filter(!flag_false) |>
    dplyr::mutate(
      dt_utc = lubridate::force_tz(detection_datetime, "UTC"),
      bin_start = lubridate::floor_date(dt_utc, unit = paste(bin_mins, "mins"))
    ) |>
    dplyr::distinct(fish_id, location_id, bin_start) |>
    dplyr::arrange(fish_id, location_id, bin_start)

  presence_runs <- presence_bins |>
    dplyr::group_by(fish_id, location_id) |>
    dplyr::mutate(
      gap = as.numeric(bin_start - dplyr::lag(bin_start), units = "secs"),
      new_run = is.na(gap) | gap > bin_mins * 60,
      run_id = cumsum(new_run)
    ) |>
    dplyr::group_by(fish_id, location_id, run_id) |>
    dplyr::summarize(
      start = min(bin_start),
      end = max(bin_start) + lubridate::minutes(bin_mins),
      n_bins_present = n(),
      duration_hours = (max(as.numeric(bin_start)) - min(as.numeric(bin_start)) + bin_mins * 60) / 3600,
      n_bins_possible = as.integer(round(duration_hours * 60 / bin_mins)),
      occupancy = n_bins_present / n_bins_possible,
      .groups = "drop"
    )

  potential_mort_runs <- presence_runs |>
    dplyr::filter(
      duration_hours > cluster_threshold,
      occupancy >= 0.8
    )

  bins <- detection.df |>
    dplyr::filter(!flag_false) |>
    dplyr::mutate(
      dt_utc = lubridate::force_tz(detection_datetime, "UTC"),
      bin_start = lubridate::floor_date(dt_utc, unit = paste(bin_mins, "mins"))
    ) |>
    dplyr::distinct(fish_id, location_id, bin_start) |>
    dplyr::mutate(t = as.numeric(bin_start)) |>
    dplyr::arrange(fish_id, location_id, bin_start)

  runs2 <- potential_mort_runs |>
    dplyr::mutate(
      run_id = dplyr::row_number(),
      end_utc = lubridate::force_tz(end, "UTC")
    )

  after_window_secs <- 3600

  after_bins <- bins |>
    dplyr::inner_join(
      runs2 |> dplyr::select(run_id, fish_id, home_location_id = location_id, end_utc),
      by = "fish_id"
    ) |>
    dplyr::filter(
      location_id != home_location_id,
      bin_start > end_utc
    ) |>
    dplyr::group_by(run_id, other_location_id = location_id) |>
    dplyr::arrange(t, .by_group = TRUE) |>
    dplyr::summarize(
      has_cluster = any((dplyr::lead(t, 2) - t) <= after_window_secs, na.rm = TRUE),
      .groups = "drop"
    )

  run_flags <- after_bins |>
    dplyr::group_by(run_id) |>
    dplyr::summarize(
      has_credible_elsewhere = any(has_cluster),
      n_other_receivers_with_cluster = sum(has_cluster),
      .groups = "drop"
    )

  runs_flagged <- potential_mort_runs |>
    dplyr::mutate(run_id = dplyr::row_number()) |>
    dplyr::left_join(run_flags, by = "run_id") |>
    dplyr::mutate(
      has_credible_elsewhere = dplyr::coalesce(has_credible_elsewhere, FALSE),
      n_other_receivers_with_cluster = dplyr::coalesce(n_other_receivers_with_cluster, 0L)
    )

  likely_dead_summary <- runs_flagged |>
    dplyr::filter(has_credible_elsewhere == FALSE) |>
    dplyr::group_by(fish_id, location_id) |>
    dplyr::summarize(
      flagged_runs_start = min(start),
      flagged_runs_end = max(end),
      flagged_runs = dplyr::n(),
      latest_flagged_start = min(start)
    )
}
