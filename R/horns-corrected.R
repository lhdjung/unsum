#' Horns Index with Sample Size Corrections
#'
#' @description Calculates the horns index accounting for sample size effects.
#'   Two versions are provided: one that accounts for finite sample constraints (h)
#'   and one that uses theoretical limits (h*).
#'
#' @details The horns index measures variance as a proportion of maximum possible
#'   variance. For finite samples, two effects modify the achievable maximum:
#'   1. Sample variance formula (Bessel's correction) affects all N
#'   2. Odd sample sizes prevent perfect 50-50 splits between extremes
#'
#' @param freqs Numeric vector with frequencies of binned observations
#' @param scale_min Numeric. Minimum value of the scale
#' @param scale_max Numeric. Maximum value of the scale
#' @param use_sample_variance Logical. If TRUE, uses sample variance (n-1).
#'   If FALSE, uses population variance (n). Default is TRUE to match
#'   standard statistical practice and CLOSURE methodology.
#' @param return_details Logical. If TRUE, returns detailed components
#'
#' @return If return_details = FALSE: the horns index (numeric, 0 to 1)
#'         If return_details = TRUE: a list with components:
#'         - h: the finite-sample horns index
#'         - h_star: the theoretical (infinite-sample) horns index
#'         - var_observed: observed variance
#'         - max_var_finite: maximum variance for this specific N
#'         - max_var_theoretical: theoretical maximum (Popoviciu bound)
#'         - n: sample size
#'         - variance_formula: "sample" or "population"
#'
#' @noRd
#'
#' @examples
#' # Example with odd N = 27 on 1-5 scale
#' freqs_odd <- c(13, 0, 0, 0, 14)  # Close to maximum variance
#' horns_corrected(freqs_odd, 1, 5)
#'
#' # Compare h and h* for small sample
#' freqs_small <- c(3, 0, 0, 0, 2)  # N=5
#' horns_corrected(freqs_small, 1, 5, return_details = TRUE)

horns_corrected <- function(
  freqs,
  scale_min,
  scale_max
) {
  # Input validation
  if (!is.numeric(freqs) || any(freqs < 0)) {
    stop("freqs must be a non-negative numeric vector")
  }
  if (!is.numeric(scale_min) || !is.numeric(scale_max)) {
    stop("scale_min and scale_max must be numeric")
  }
  if (scale_min >= scale_max) {
    stop("scale_min must be less than scale_max")
  }

  n <- sum(freqs)
  k <- length(freqs)

  # Check inputs for consistency
  if (k != length(scale_min:scale_max)) {
    stop(sprintf(
      "Length of freqs (%d) doesn't match scale length (%d to %d)",
      k,
      scale_min,
      scale_max
    ))
  }

  # Handle edge cases where variance is undefined
  if (n %in% c(0, 1)) {
    return(NA_real_)
  }

  scale_points <- seq_along(freqs)
  freqs_relative <- freqs / n
  mean_observed <- sum(scale_points * freqs_relative)

  # Sum of squared deviations
  sum_of_squares <- sum(freqs * (scale_points - mean_observed)^2)
  var_observed <- sum_of_squares / (n - 1)

  # With an even sample size, a perfectly even split is possible, and the mean
  # is identical to the scale midpoint
  if (n %% 2 == 0) {
    n1 <- n / 2
    n2 <- n1

    mean_at_max <- (k + 1) / 2
  } else {
    # With an odd sample size, the split is uneven: one "horn" has one more
    # observation than the other, so the mean is slightly off the midpoint
    n1 <- (n - 1) / 2
    n2 <- (n + 1) / 2

    mean_at_max <- (n1 + n2 * k) / n
  }

  sum_of_squares_max <- n1 * (1 - mean_at_max)^2 + n2 * (k - mean_at_max)^2
  var_max <- sum_of_squares_max / (n - 1)

  var_observed / var_max
}


#' Calculate h* (Theoretical Horns Index)
#'
#' @description The theoretical horns index that doesn't account for finite
#'   sample constraints. This represents the limiting case as the sample size
#'   approaches infinity.
#'
#' @inheritParams horns
#'
#' @noRd

horns_star <- function(freqs, scale_min, scale_max) {
  n <- sum(freqs)
  k <- length(freqs)

  # Calculate variance using population formula (limiting case)
  scale_points <- seq_along(freqs)
  freqs_relative <- freqs / n
  mean_observed <- sum(scale_points * freqs_relative)
  var_observed <- sum(freqs_relative * (scale_points - mean_observed)^2)

  # Theoretical maximum (Popoviciu bound)
  max_var_theoretical <- (k - 1)^2 / 4

  # h* is simply the ratio using theoretical maximum
  out <- var_observed / max_var_theoretical

  min(out, 1)
}
