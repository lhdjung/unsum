#' Count CLOSURE samples in advance
#'
#' @description Determine how many samples [`closure_generate()`] would find for
#'   a given set of summary statistics.
#'
#'   - `closure_count_all()` counts all CLOSURE samples that correspond to the
#'   input summary statistics, but without actually generating the samples. This
#'   is much faster than `closure_generate()` if there are many results.
#'   - `closure_count_initial()` only counts the first round of samples, from
#'   which all other ones would be generated. Based on scale range only.
#'
#'   This can help predict how much time [`closure_generate()`] would take, and
#'   avoid prohibitively long runs.
#'
#' @inheritParams closure_generate
#' @param scale_min,scale_max Integers (length 1 each). Minimum and maximum of
#'   the scales to which the reported statistics refer.
#'
#' @return Integer (length 1).
#'
#' @include utils.R generate.R extendr-wrappers.R
#'
#' @export
#'
#' @examples
#' closure_count_all(
#'   mean = "3.5",
#'   sd = "1.7",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' closure_count_initial(scale_min = 1, scale_max = 5)

closure_count_all <- function(
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  rounding = "up_or_down",
  threshold = 5
) {
  # Same procedure as in `generate_from_mean_sd_n()`, and therefore
  # `closure_generate()`
  check_single(mean, "character")
  check_single(sd, "character")
  check_single(n, c("double", "integer"))
  check_single(scale_min, c("double", "integer"))
  check_single(scale_max, c("double", "integer"))
  check_single(rounding, "character")
  check_single(threshold, c("double", "integer"))

  check_whole_number(n)
  check_whole_number(scale_min)
  check_whole_number(scale_max)

  check_scale(scale_min, scale_max, as.numeric(mean))

  # See `generate_from_mean_sd_n()` for why the interval centers are used
  mean_sd_unrounded <- unround_center_error(
    x = c(mean, sd),
    rounding = rounding,
    threshold = threshold
  )

  # Call into the Rust implementation
  count_closure_combinations(
    mean = mean_sd_unrounded$center[1],
    sd = mean_sd_unrounded$center[2],
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    rounding_error_mean = mean_sd_unrounded$error[1],
    rounding_error_sd = mean_sd_unrounded$error[2]
  )
}


# (By Claude:) Each combination starts with two numbers i,j where scale_min <= i
# <= j <= scale_max
# This is equivalent to choosing 2 numbers with replacement where order doesn't
# matter The formula is: (n+1) * n / 2 where n is the range size

#' @rdname closure_count_all
#' @export
closure_count_initial <- function(scale_min, scale_max) {
  check_single(scale_min, c("double", "integer"))
  check_single(scale_max, c("double", "integer"))

  check_scale(scale_min, scale_max)

  range_size <- scale_max - scale_min + 1

  (range_size * (range_size + 1)) / 2
}
