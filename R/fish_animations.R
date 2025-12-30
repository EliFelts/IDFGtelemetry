#' Build animations showing interpolated fish movements between acoustic receiver deployments
#'
#' @param hourly_detections A data frame of hourly fish locations returned from [interpolate_hourly()]
#' @param duration length of animation, in seconds; nframes=duration * fps
#' @param fps frames per second
#' @param trail_seconds seconds of wake showing trail behind current point (converted to proportion internally)
#' @param waterbody_shape sf polygon of waterbody, defaults to Lake Pend Oreille currently
#' @param out_dir Directory where animations will be saved; if it doesn't already exist, an "animations" folder will be created by default
#' @param filename Animation file name
#' @return An animation in .webm format
#' @export
#'
fish_animation <- function(hourly_detections, duration = 60,
                           fps = 20, trail_seconds = 2,
                           waterbody_shape = lpo.shp,
                           out_dir = "animations",
                           filename = NULL) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (is.null(filename)) {
    base <- if (length(unique(hourly_detections$fish_id)) == 1) {
      paste0("fish_", unique(hourly_detections$fish_id))
    } else {
      "fish_animation"
    }
    filename <- paste0(base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  outfile <- file.path(out_dir, paste0(filename, ".", "webm"))

  static_map <- hourly_detections %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = waterbody_shape) +
    ggplot2::geom_point(
      aes(
        x = det_long, y = det_lat,
        fill = fish_id
      ),
      pch = 21,
      size = 4,
      show.legend = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = element_text(
      color = "red",
      size = 28,
      face = "bold",
      hjust = 0.5
    ))

  wake_prop <- max(min(trail_seconds / duration, 1), 0)

  nframes <- duration * fps


  animation <- static_map +
    gganimate::transition_time(time = detection_hour) +
    labs(title = 'Date: {format(frame_time,format= "%B %d %Y")}') +
    shadow_wake(wake_length = wake_prop)

  animate(animation,
    fps = fps, nframes = nframes,
    height = 6, width = 12, units = "in",
    res = 150,
    renderer = av_renderer(outfile)
  )

  return(invisible(outfile))
}
