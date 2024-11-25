
#' Summarize CLOSURE data
#'
#' @description Use `closure_summarize()` to compute frequencies of data coming
#'   from [`closure_combine()`] or [`closure_pivot_longer()`].
#'
#' @param data CLOSURE data frame.
#'
#' @return Tibble (data frame) with these columns:
#'  - `value`: integer. Scale values.
#'  - `f_absolute`: Absolute frequencies of the scale values in `data`.
#'  - `f_relative`: Relative frequencies of the scale values in `data`.
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
    abort_not_closure_data(allow_pivot = TRUE)
  }

  # Group by scale value, transforming the data into a list of data frames.
  # Mapping `nrow()` to the list returns the number of rows in each data frame,
  # i.e., the number of times that the scale values appear in the combinations.
  f_absolute <- data %>%
    split(data$value) %>%
    vapply(
      FUN       = nrow,
      FUN.VALUE = integer(1),
      USE.NAMES = TRUE
    )

  f_relative <- f_absolute / sum(f_absolute)
  value <- as.integer(names(f_absolute))
  value_completed <- seq(from = value[1], to = value[length(value)])

  # If each possible value is instantiated in the values that were actually
  # found, the results are complete and will be returned here. If not, the
  # uninstantiated values must be added to `value`, and their frequencies of
  # zero to `f_absolute` and `f_relative`. This is what the rest of the function
  # will do.
  if (length(value) == length(value_completed)) {
    out <- tibble::tibble(
      value,
      f_absolute,
      f_relative
    ) %>%
      add_class("closure_summarize")
    return(out)
  }

  # At which indices in the complete vector of possible values are those values
  # that were actually found?
  indices_found <- which(value_completed %in% value)

  # Construct full-length vectors where each value is zero
  f_absolute_completed <- integer(length(value_completed))
  f_relative_completed <- double(length(value_completed))

  # Fill in the non-zero values where appropriate
  f_absolute_completed[indices_found] <- f_absolute
  f_relative_completed[indices_found] <- f_relative

  tibble::tibble(
    value      = value_completed,
    f_absolute = f_absolute_completed,
    f_relative = f_relative_completed
  ) %>%
    add_class("closure_summarize")
}
