
#' Count initial CLOSURE combinations
#'
#' @param min_scale,max_scale Integers (length 1 each). Minimum and maximum of
#'   the scales to which the reported statistics refer.
#'
#' @return
#' @export
#'
#' @examples

# Each combination starts with two numbers i,j where min_scale ≤ i ≤ j ≤ max_scale
# This is equivalent to choosing 2 numbers with replacement where order doesn't matter
# The formula is: (n+1) * n / 2 where n is the range size

# This make more sense on the Rust level, maybe even exported from closure-core?

closure_count_combinations_initial <- function(min_scale, max_scale) {
    range_size <- max_scale - min_scale + 1
    (range_size * (range_size + 1)) / 2
}
