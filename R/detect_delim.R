detect_delim <- function(file) {
  header <- readr::read_lines(file, n_max = 1)

  counts <- c(
    "," = str_count(header, fixed(",")),
    ";" = str_count(header, fixed(";")),
    "\t" = str_count(header, fixed("\t")),
    "|" = str_count(header, fixed("|"))
  )

  names(which.max(counts))
}
