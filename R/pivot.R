
#' Pivot CLOSURE data from wide to long
#'
#' @description Data coming from `closure_read()` can be reshaped using
#'   `closure_pivot_longer()`. This makes it easier to assess which "n" variable
#'   holds which combinations.
#'
#'   This function is a thin wrapper around [`tidyr::pivot_longer()`].
#'
#' @param data Data frame returned by `closure_read()`.
#' @param cols_vary String (length 1). How to arrange rows?
#'  - If `"slowest"`, the default, values from the same original `"n"` columns
#'   are kept together.
#'  - If `"fastest"`, the combinations (or rows in the original `data`) are kept
#'   together, so that `"n"` varies after each row.
#'
#' @return Tibble (data frame) with two columns:
#'  - `"n"`: integer. Numbers from the column names of `data`, like `1` for
#'   `"n1"`, `2` for `"n2"`, etc.
#'  - `"value"`: integer. Combination components, i.e., all values from `data`.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
closure_pivot_longer <- function(data, cols_vary = c("slowest", "fastest")) {

  check_closure_data(data)

  # Pivot all the values into a single column, "value". The "n" column says
  # which original variable they are from. The rows are ordered by "n" via
  # `cols_vary = "slowest"` by default to keep the "n" variables together.
  out <- tidyr::pivot_longer(
    data      = data,
    cols      = tidyr::everything(),
    cols_vary = cols_vary,
    names_to  = "n",
    values_to = "value"
  )

  # Reduce the `n` column to its only informative component by coercing it to an
  # integer column of the numbers after the "n" in the original variables: n1,
  # n2, n3, etc.
  out$n <- as.integer(sub("n", "", out$n))

  # Add a class that can later tell `closure_plot()` that the data is a product
  # of `closure_pivot_longer()`. Then, return the data.
  `class<-`(out, value = c("closure_pivot_longer", class(out)))
}
