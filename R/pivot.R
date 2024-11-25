
#' Pivot CLOSURE data from wide to long
#'
#' @description Data coming from [`closure_combine()`] can be reshaped using
#'   `closure_pivot_longer()`. This turns CLOSURE output into tidy data.
#'
#'   It is a wrapper around [`tidyr::pivot_longer()`] tailored to CLOSURE data.
#'
#' @param data Data frame returned by [`closure_combine()`].
#' @param cols_vary String (length 1). How to arrange rows?
#'  - If `"slowest"`, the default, values from the same original `"n"` columns
#'   are kept together.
#'  - If `"fastest"`, the combinations (or rows in the original `data`) are kept
#'   together, so that `"n"` varies after each row.
#'
#' @details The present function differs from [`tidyr::pivot_longer()`] in these
#'   ways:
#'  - It checks whether `data` are CLOSURE output.
#'  - It always includes all columns.
#'  - By default (`cols_vary = "slowest"`), it clusters the output by the
#'   original columns, such as `"n1"` and `"n2"`. This preserves the natural
#'   grouping of the data.
#'  - It transforms the `"n"` column to integer, like `1` for `"n1"` and `2` for
#'   `"n2"`. The `"n"` prefix would be redundant and distract from the numeric
#'   values.
#'  - It adds the `"closure_pivot_longer"` class to the output. This will inform
#'   downstream functions, such as [`closure_summarize()`] and
#'   [`closure_plot()`].
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

  check_closure_combine(data)

  # Pivot all the values into a single column, "value". The "n" column says
  # which original variable they are from. The rows are ordered by "n" via
  # `cols_vary = "slowest"` by default to keep the "n" variables together. Also,
  # the "n" column is reduced to its only informative component by coercing it
  # to an integer column of the numbers after the "n" in the original variables:
  # n1, n2, n3, etc. This uses `n_to_integer()`, a helper written in Rust. Then,
  # add a class that can later tell `closure_summarize()` and `closure_plot()`
  # that the data is a product of this function.
  data %>%
    tidyr::pivot_longer(
      cols            = tidyr::everything(),
      cols_vary       = cols_vary,
      names_transform = function(x) as.integer(sub("n", "", x)),
      names_to        = "n",
      values_to       = "value"
    ) %>%
    add_class("closure_pivot_longer")
}
