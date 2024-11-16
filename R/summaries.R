
#' Summarize CLOSURE data
#'
#' @description Use `closure_count()` to count frequencies of data coming from
#'   `closure_read()`.
#'
#'   Should also support data coming from [`closure_pivot_longer()`] in the
#'   future.
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
closure_count <- function(data) {

  if (inherits(data, "closure_data")) {
    data <- closure_pivot_longer(data)
  } else if (!inherits(data, "closure_pivot_longer")) {
    abort_not_closure_data(action = "count values of", allow_pivot = TRUE)
  }

  # Group by scale value, transforming the data into a list of data frames
  data <- split(data, data$value)

  n_absolute <- vapply(
    X = data,
    FUN = nrow,
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
