#' Heuristic to predict CLOSURE runtime
#'
#' @description Before you run [`closure_generate()`], you may want to get a
#'   sense of the time it will take to run. Use `closure_gauge_complexity()` to
#'   compute a heuristics-based complexity score. For reference, here is how it
#'   determines the messages in `closure_generate()`:
#'
#' \tabular{ll}{
#'   \strong{Value} \tab \strong{Message} \cr
#'   if < 1       \tab (no message)                 \cr
#'   else if < 2  \tab "Just a second..."           \cr
#'   else if < 3  \tab "This could take a minute..."\cr
#'   else         \tab "NOTE: Long runtime ahead!"
#' }
#'
#' @details The result of this function is hard to interpret. All it can do is
#'   to convey an idea about the likely runtime of CLOSURE. This is because the
#'   input parameters interact in highly dynamic ways, which makes prediction
#'   difficult.
#'
#'   In addition, even progress bars or updates at regular intervals (e.g., "10%
#'   complete") prove to be extremely challenging: the Rust code computes
#'   CLOSURE results in parallel, which makes it hard to get an overview of the
#'   total progress across all cores; and especially to display such information
#'   on the R level.
#'
#' @inheritParams closure_generate
#'
#' @returns Numeric (length 1).
#'
#' @export
#'
#' @examples
#' # Low SD, N, and scale range:
#' closure_gauge_complexity(
#'   mean = 2.55,
#'   sd = 0.85,
#'   n = 84,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Somewhat higher:
#' closure_gauge_complexity(
#'   mean = 4.26,
#'   sd = 1.58,
#'   n = 100,
#'   scale_min = 1,
#'   scale_max = 7
#' )
#'
#' # Very high:
#' closure_gauge_complexity(
#'   mean = 3.81,
#'   sd = 3.09,
#'   n = 156,
#'   scale_min = 1,
#'   scale_max = 7
#' )

closure_gauge_complexity <- function(mean, sd, n, scale_min, scale_max) {

  mean <- as.numeric(mean)
  sd <- as.numeric(sd)

  # Scale range is THE dominant factor - empirically verified
  range_size <- scale_max - scale_min + 1

  # 1. Initial combinations (quadratic in range_size)
  initial_combinations <- (range_size * (range_size + 1)) / 2

  # 2. Scale range factor (this has exponential impact through the search tree)
  scale_factor <- if (range_size <= 3) {
    0.1                      # trivial cases
  } else if (range_size <= 5) {
    1.0                      # baseline
  } else if (range_size <= 7) {
    50.0                     # under a minute
  } else if (range_size <= 9) {
    500.0                    # extrapolated
  } else {
    5000.0                   # very large ranges
  }

  # 3. Sample size factor (tree depth effect)
  n_factor <- if (n <= 20) {
    0.5
  } else if (n <= 50) {
    1.0                      # baseline
  } else if (n <= 100) {
    3.0
  } else if (n <= 200) {
    10.0
  } else {
    50.0
  }

  # 4. SD constraint factor (loose constraints = less pruning = harder)
  # Your sd=2.0 case was loose and took time
  sd_factor <- if (sd <= 0.5) {
    0.1                      # very tight = easy
  } else if (sd <= 1.0) {
    0.5                      # tight
  } else if (sd <= 1.5) {
    1.0                      # moderate
  } else if (sd <= 2.0) {
    2.0                      # loose
  } else {
    5.0                      # very loose
  }

  # 5. Mean extremeness (extreme means are harder to achieve)
  scale_center <- (scale_min + scale_max) / 2
  mean_distance_from_center <- abs(mean - scale_center)
  max_distance <- (scale_max - scale_min) / 2
  mean_extremeness <- mean_distance_from_center / max_distance

  mean_factor <- if (mean_extremeness < 0.2) {
    1.0                      # central means are easier
  } else if (mean_extremeness < 0.5) {
    1.5
  } else {
    3.0                      # extreme means are harder
  }

  # 6. Base complexity calculation
  base_complexity <- scale_factor * n_factor * sd_factor * mean_factor

  # 7. Convert to log scale for interpretability
  log10(base_complexity)
}
