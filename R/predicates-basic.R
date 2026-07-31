#' @include utils.R

# Test CLOSURE / SPRITE results for being empty. Assumes that `data` is output
# of a generator or reader function but doesn't check this, so use with care.
is_empty_basic <- function(data, technique) {
  check_generator_output(data, technique, allow_empty = TRUE)
  data$metrics_main$samples_all == 0
}

# Stopgap for now
is_empty <- function(data) {
  technique <- data$inputs$technique
  check_generator_output(data, technique, allow_empty = TRUE)
  data$metrics_main$samples_all == 0
}


is_symmetric_basic <- function(
  data,
  technique,
  tolerance = .Machine$double.eps^0.5,
  metric = c("f_count", "f_relative")
) {
  check_generator_output(data, technique, allow_empty = TRUE)
  metric <- arg_match_in_export(metric)

  freqs <- data$frequency[data$frequency$samples == "all", ][[metric]]
  scale_length <- length(freqs)

  if (scale_length < 3) {
    if (scale_length == 2) {
      return(near(freqs[1], freqs[2], tolerance))
    }
    return(TRUE)
  }

  indices_all <- seq_len(scale_length)
  center <- (scale_length / 2) + 0.5

  indices_lo <- indices_all[indices_all < center]
  indices_hi <- indices_all[indices_all > center]

  # Check for equality within each pair of frequencies that have the same
  # position when starting at the extremes and moving towards the center.
  # Tolerance of comparison can be specified by the user.
  mirror_equal <- near(
    freqs[indices_lo],
    rev(freqs[indices_hi]),
    tol = tolerance
  )

  all(mirror_equal)
}
