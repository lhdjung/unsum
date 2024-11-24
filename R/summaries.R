
#' Summarize CLOSURE data
#'
#' @description Use `closure_summarize()` to compute frequencies of data coming
#'   from `closure_read()` or [`closure_pivot_longer()`].
#'
#' @param data CLOSURE data frame.
#'
#' @return Tibble (data frame) with these columns:
#'  - `value`: integer. Scale values.
#'  - `n_absolute`: Absolute frequencies of the scale values in `data`.
#'  - `n_relative`: Relative frequencies of the scale values in `data`.
#'
#' @export
#'
#' @examples
closure_summarize <- function(data) {

  if (inherits(data, "closure_combine")) {
    data <- closure_pivot_longer(data)
  } else if (inherits(data, "closure_pivot_longer")) {
    check_closure_pivot_longer_unaltered(data)
  } else {
    abort_not_closure_combine(allow_pivot = TRUE)
  }

  # Group by scale value, transforming the data into a list of data frames.
  # Mapping `nrow()` to the list returns the number of rows in each data frame,
  # i.e., the number of times that the scale values appear in the combinations.
  n_absolute <- data %>%
    split(data$value) %>%
    vapply(
      FUN       = nrow,
      FUN.VALUE = integer(1),
      USE.NAMES = TRUE
    )

  n_relative <- n_absolute / sum(n_absolute)

  value <- as.integer(names(n_absolute))

  tibble::tibble(
    value,
    n_absolute,
    n_relative
  )

}
