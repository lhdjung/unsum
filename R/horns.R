#' Horns index (\eqn{h})
#'
#' @description `horns()` measures the dispersion in ordinal data based on scale
#'   limits or min/max values. The result is the actual variance as a proportion
#'   of the maximum possible variance. It ranges from 0 to 1:
#'   - 0 means no variance, i.e., all observations have the same value.
#'   - 1 means that the observations are evenly split between the extremes, with
#'   none in between.
#'
#'   `horns_uniform()` computes the value that `horns()` would return for a
#'   uniform distribution within given scale limits. This can be useful as a
#'   point of reference for `horns()`.
#'
#'   These two functions correspond to the `horns` and `horns_uniform` columns
#'   in [`closure_generate()`].
#'
#'   `horns_rescaled()` is a version of `horns()` that  is normalized by scale
#'   length, such that `0.5` always indicates a uniform distribution,
#'   independent of the number of scale points. It is meant to enable comparison
#'   across scales of different lengths, but it is harder to interpret for an
#'   individual scale. This makes it unlikely to be useful in most cases. Even
#'   so, the range and the meaning of `0` and `1` are the same as for `horns()`.
#'
#' @param freqs Numeric. Vector with the frequencies (relative or absolute) of
#'   binned observations; e.g., a vector with 5 elements for a 1-5 scale.
#' @inheritParams closure_generate
#'
#' @details The horns index \eqn{h} is defined as:
#'
#'   \deqn{
#'      h = 4 \frac
#'      {\sum_{i=1}^{k} f_i (i - \bar{s})^2}
#'      {(k - 1)^2}
#'   }
#'
#'   where \eqn{k} is the number of scale points (i.e., the length of `freqs`
#'   here), \eqn{f_i} is the relative frequency of the \eqn{i}th point on an
#'   integer scale from \eqn{1} to \eqn{k}, and \eqn{\bar{s}} is the weighted
#'   mean frequency. The mean is derived as follows:
#'
#'   \deqn{ \bar{s} = \sum_{i=1}^{k} i \ f_i }
#'
#'   Note that \eqn{h} only depends on the frequency distribution, not the
#'   actual values of the scale points. This is why both formulas invariably use
#'   a scale from \eqn{1} to \eqn{k}. The only reason why the `horns()` function
#'   still takes `scale_min` and `scale_max` arguments is safety: if `freqs` is
#'   misstated such that its length is different from the number of points on
#'   the scale implied by those two arguments, there will be an error.
#'
#'   ## Uniform distribution
#'
#'   Although `horns_uniform()` is implemented as a wrapper around `horns()`
#'   that constructs a perfect uniform distribution internally, an equivalent
#'   closed-form solution can be given as
#'
#'   \deqn{
#'      h_u = 4 \frac
#'      {\sum_{i=1}^{k} (i - \bar{s})^2}
#'      {k \ (k - 1)^2}
#'   }
#'
#'   In the uniform case, the weighted mean is simply the scale midpoint, i.e.,
#'
#'   \deqn{
#'      \bar{s}_u
#'      = \sum_{i=1}^{k} \frac{i}{k}
#'      = \frac{k + 1}{2}
#'   }
#'
#'   ## "Horns of no confidence"
#'
#'   The term *horns index* was inspired by [Heathers
#'   (2017)](https://jamesheathers.medium.com/sprite-case-study-3-soup-is-good-albeit-extremely-confusing-food-96ea526c488d)
#'   which defines the "horns of no confidence" as a reconstructed sample "where
#'   an incorrect, impossible or unlikely value set has all its constituents
#'   stacked into its highest or lowest bins to try meet a ludicrously high SD".
#'   In its purest form, this is a case where \eqn{h = 1}, so `horns()` would
#'   return `1`. However, note that the implications for the plausibility of any
#'   given set of summary statistics depend on the substantive context of the
#'   data ([Heathers et al. 2018](https://peerj.com/preprints/26968/)).
#'
#'   ## Rust implementation
#'
#'   The `metrics_horns` tibble that is part of the output of
#'   [`closure_generate()`] is not based on the R functions presented here.
#'   Instead, it relies on efficient Rust implementations of the above formulas.
#'   These Rust functions are part of
#'   [closure-core](https://crates.io/crates/closure-core), which mainly
#'   implements CLOSURE but does not currently export horns functions for users.
#'
#' @returns Numeric (length 1).
#'
#' @export
#'
#' @examples
#' # For simplicity, all examples use a 1-5 scale and a total N of 300.
#'
#' # ---- With all values at the extremes
#'
#' horns(freqs = c(300, 0, 0, 0, 0), scale_min = 1, scale_max = 5)
#'
#' horns(c(150, 0, 0, 0, 150), 1, 5)
#'
#' horns(c(100, 0, 0, 0, 200), 1, 5)
#'
#'
#' # ---- With some values in between
#'
#' horns(c(60, 60, 60, 60, 60), 1, 5)
#'
#' horns(c(200, 50, 30, 20, 0), 1, 5)
#'
#' horns(c(150, 100, 50, 0, 0), 1, 5)
#'
#' horns(c(100, 40, 20, 40, 100), 1, 5)

horns <- function(freqs, scale_min, scale_max) {
  check_type(freqs, "double")

  check_value(scale_min, "double")
  check_value(scale_max, "double")

  check_scale(scale_min, scale_max)

  scale_complete <- seq_along(freqs)
  k <- length(scale_complete)

  # Error if arguments are incompatible in terms of length
  if (k != length(scale_min:scale_max)) {
    cli::cli_abort(
      message = c(
        "Scale implied by `scale_min` ({scale_min}) \
        and `scale_max` ({scale_max}) must match `freqs` in length.",
        "x" = " Scale: length {length(scale_complete)} (i.e., {scale_min} \
        through {scale_max})",
        "x" = "`freqs`: length {length(freqs)}"
      ),
      call = rlang::caller_env()
    )
  }

  freqs_relative <- freqs / sum(freqs)

  # Using a 1-k scale and weighting it by the relative frequencies
  scale_mean <- sum(scale_complete * freqs_relative)

  # Weighted sum of squared deviations
  numerator <- sum(freqs_relative * (scale_complete - scale_mean)^2)

  # Maximum possible variance given scale limits
  # (inflated by a factor of 4, which is canceled out below)
  denominator <- (k - 1)^2

  4 * numerator / denominator
}


#' @rdname horns
#' @export

# The function uses `1` for the uniform frequency, but any other non-zero
# definite number (no `NA`, `Inf`, or `NaN`) would lead to the same results.
horns_uniform <- function(scale_min, scale_max) {
  horns(
    freqs = rep(1, length(scale_min:scale_max)),
    scale_min = scale_min,
    scale_max = scale_max
  )
}


# # Closed-form solution of `horns_uniform()` with a 1-7 scale:
# h_u == 4 * (1 / 7) * sum(c(3, 2, 1, 0, 1, 2, 3)^2) / (7 - 1)^2
#     == 4 * sum(c(3, 2, 1, 0, 1, 2, 3)^2) / (7 * (7 - 1)^2)
#     == 0.4444444
#
# # Not necessary, just interesting:
# horns_uniform_closed <- function(scale_min, scale_max) {
#   check_scale(scale_min, scale_max)
#
#   scale_complete <- seq_along(scale_min:scale_max)
#   k <- length(scale_complete)
#
#   scale_mean <- (k + 1) / 2
#
#   deviations <- scale_complete - scale_mean
#
#   4 * sum(deviations^2) / (k * (k - 1)^2)
# }


#' @rdname horns
#' @export

horns_rescaled <- function(freqs, scale_min, scale_max) {
  # Actual horns index and reference value for uniform distribution
  h_actual <- horns(freqs, scale_min, scale_max)
  h_uniform <- horns_uniform(scale_min, scale_max)

  if (h_actual <= h_uniform) {
    # Map [0, h_uniform] to [0, 0.5]
    0.5 * (h_actual / h_uniform)
  } else {
    # Map [h_uniform, 1] to [0.5, 1]
    0.5 + 0.5 * ((h_actual - h_uniform) / (1 - h_uniform))
  }
}

