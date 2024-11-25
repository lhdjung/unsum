
#' Count initial CLOSURE combinations
#'
#' @param scale_min,scale_max Integers (length 1 each). Minimum and maximum of
#'   the scales to which the reported statistics refer.
#'
#' @return
#' @export
#'
#' @examples

# (By Claude:) Each combination starts with two numbers i,j where scale_min <= i
# <= j <= scale_max
# This is equivalent to choosing 2 numbers with replacement where order doesn't
# matter The formula is: (n+1) * n / 2 where n is the range size

closure_count_initial <- function(scale_min, scale_max) {
    range_size <- scale_max - scale_min + 1
    (range_size * (range_size + 1)) / 2
}
