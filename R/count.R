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
  check_value(mean, "character")
  check_value(sd, "character")
  check_value(n, c("double", "integer"))
  check_value(scale_min, c("double", "integer"))
  check_value(scale_max, c("double", "integer"))
  check_value(rounding, "character")
  check_value(threshold, c("double", "integer"))

  mean_num <- as.numeric(mean)
  sd_num <- as.numeric(sd)

  check_scale(scale_min, scale_max, mean_num)

  mean_sd_unrounded <- roundwork::unround(
    x = c(mean, sd),
    rounding = rounding,
    threshold = threshold
  )

  rounding_error_mean <- mean_num - mean_sd_unrounded$lower[1]
  rounding_error_sd <- sd_num - mean_sd_unrounded$lower[2]

  # Call into the Rust implementation
  count_closure_combinations(
    mean = mean_num,
    sd = sd_num,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    rounding_error_mean = rounding_error_mean,
    rounding_error_sd = rounding_error_sd
  )
}


# (By Claude:) Each combination starts with two numbers i,j where scale_min <= i
# <= j <= scale_max
# This is equivalent to choosing 2 numbers with replacement where order doesn't
# matter The formula is: (n+1) * n / 2 where n is the range size

#' @rdname closure_count_all
#' @export
closure_count_initial <- function(scale_min, scale_max) {
  check_value(scale_min, c("double", "integer"))
  check_value(scale_max, c("double", "integer"))

  check_scale(scale_min, scale_max)

  range_size <- scale_max - scale_min + 1

  (range_size * (range_size + 1)) / 2
}
