read_detections <- function(file) {
  if (file.info(file)$size == 0) {
    return(NULL)
  }

  delim <- detect_delim(file)

  df <- readr::read_delim(
    file,
    delim = delim,
    col_types = readr::cols(
      `Date and Time (UTC)` = readr::col_character(), # FORCE character
      Receiver = readr::col_character(),
      Transmitter = readr::col_character(),
      `Sensor Value` = readr::col_double(),
      .default = readr::col_skip()
    ),
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  )

  if (nrow(df) == 0) {
    return(NULL)
  }

  # schema check
  required <- c("Date and Time (UTC)", "Receiver", "Transmitter", "Sensor Value")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    warning(
      "Bad schema in ", basename(file), " (likely delimiter): missing ",
      paste(missing, collapse = ", ")
    )
    return(NULL)
  }

  df %>%
    transmute(
      file = basename(file),
      delim = delim,
      dt_raw = `Date and Time (UTC)`,
      internal_receiver_id = Receiver,
      acoustic_tag_id = Transmitter,
      raw_sensor = `Sensor Value`
    )
}
