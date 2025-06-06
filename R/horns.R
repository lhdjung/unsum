#' Horns index (\eqn{h})
#'
#' @description `horns()` measures the dispersion in a sample of clamped
#'   observations based on the scale limits. It ranges from 0 to 1:
#'   - 0 means no variation, i.e., all observations have the same value.
#'   - 1 means that the observations are evenly split between the extremes, with
#'   none in between.
#'
#'   `horns_uniform()` computes the value that `horns()` would return for a
#'   uniform distribution within given scale limits. This can be useful as a
#'   point of reference for `horns()`.
#'
#'   These two functions create the `horns` and `horns_uniform` columns in
#'   [`closure_generate()`].
#'
#'   `horns_rescaled()` is a version of `horns()` that  is normalized by scale
#'   length, such that `0.5` always indicates a uniform distribution,
#'   independent of the number of scale points. It is meant to enable comparison
#'   across scales of different lengths, but it is harder to interpret for an
#'   individual scale. Even so, the range and the meaning of `0` and `1` are the
#'   same as for `horns()`.
#'
#' @param freqs Numeric. Vector with the frequencies (relative or absolute) of
#'   binned observations; e.g., a vector with 5 elements for a 1-5 scale.
#' @param scale_min,scale_max Numeric (length 1 each). Minimum and maximum of
#'   the scale on which the values were measured. These can be lower and upper
#'   bounds (e.g., with a 1-5 Likert scale) or empirical min and max reported in
#'   an article. The latter should be preferred if available because they
#'   constrain the scale further.
#'
#' @details The horns index \eqn{h} is defined as:
#'
#'   \deqn{
#'      h = \frac
#'      {\sum_{i=1}^{k} f_i (s_i - \bar{s})^2}
#'      {\frac{1}{4} (s_{\max} - s_{\min})^2}
#'   }
#'
#'   where \eqn{k} is the number of scale points (i.e., the length of `freqs`
#'   here), \eqn{f_i} is the relative frequency of the \eqn{i}th scale point,
#'   \eqn{s_i}; \eqn{\bar{s}} is the sample mean, \eqn{s_{\max}} is the upper
#'   bound of the scale, and \eqn{s_{\min}} is its lower bound.
#'
#'   Its name was inspired by [Heathers
#'   (2017a)](https://jamesheathers.medium.com/sprite-case-study-3-soup-is-good-albeit-extremely-confusing-food-96ea526c488d)
#'   which defines the "horns of no confidence" as a reconstructed sample "where
#'   an incorrect, impossible or unlikely value set has all its constituents
#'   stacked into its highest or lowest bins to try meet a ludicrously high SD".
#'   In its purest form, this is a case where `horns()` returns `1`. However,
#'   note that the implications for the plausibility of any given set of summary
#'   statistics depend on the substantive context of the data ([Heathers
#'   2017b](https://jamesheathers.medium.com/sprite-interlude-the-umbrella-graph-connecting-grim-and-sprite-also-brunch-sucks-1266c629c974)).
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

  scale_complete <- scale_min:scale_max

  # Error if arguments are incompatible in terms of length
  if (length(scale_complete) != length(freqs)) {
    cli::cli_abort(c(
      "Scale implied by `scale_min` ({scale_min}) \
      and `scale_max` ({scale_max}) must match `freqs` in length.",
      "x" = " Scale: length {length(scale_complete)} (i.e., {scale_min} \
      through {scale_max})",
      "x" = "`freqs`: length {length(freqs)}"
    ))
  }

  freqs_relative <- freqs / sum(freqs)

  scale_mean <- sum(freqs_relative * scale_complete)

  # Weighted sum of squared deviations
  numerator <- sum(freqs_relative * (scale_complete - scale_mean)^2)

  # Maximum possible variance given scale limits
  denominator <- ((scale_max - scale_min)^2) / 4

  numerator / denominator
}


#' @rdname horns
#' @export

# The function uses `1` for the uniform frequency, but any other non-zero
# definite number (no `NA`, no `NaN`) would lead to the same results.
horns_uniform <- function(scale_min, scale_max) {
  horns(
    freqs = rep(1, length(scale_min:scale_max)),
    scale_min = scale_min,
    scale_max = scale_max
  )
}


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
