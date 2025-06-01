#' Count CLOSURE samples in advance
#'
#' @description Determine how many samples [`closure_combine()`] would find for
#'   a given set of summary statistics.
#'
#'   - `closure_count_initial()` only counts the first round of samples, from
#'   which all other ones would be generated.
#'   - There is currently no `closure_count_all()` function.
#'
#'   This can help predict how much time [`closure_combine()`] would take, and
#'   avoid prohibitively long runs.
#'
#' @param scale_min,scale_max Integers (length 1 each). Minimum and maximum of
#'   the scales to which the reported statistics refer.
#'
#' @return Integer (length 1).
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' closure_count_initial(scale_min = 1, scale_max = 5)

# (By Claude:) Each combination starts with two numbers i,j where scale_min <= i
# <= j <= scale_max
# This is equivalent to choosing 2 numbers with replacement where order doesn't
# matter The formula is: (n+1) * n / 2 where n is the range size

closure_count_initial <- function(scale_min, scale_max) {
  check_value(scale_min, "double")
  check_value(scale_max, "double")

  check_scale(scale_min, scale_max)

  range_size <- scale_max - scale_min + 1

  as.integer(
    (range_size * (range_size + 1)) / 2
  )
}
