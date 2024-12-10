
#' Summarize CLOSURE data
#'
#' @description Use `closure_summarize()` to compute frequencies of values in
#'   CLOSURE results.
#'
#' @param data List returned by [`closure_combine()`] or
#'   [`closure_pivot_longer()`].
#'
#' @return Tibble (data frame) with these columns:
#'  - `value`: integer. Scale values.
#'  - `f_absolute`: Absolute frequencies of the scale values in `data`.
#'  - `f_relative`: Relative frequencies of the scale values in `data`.
#'
#' @export
#'
#' @examples
#' # Create CLOSURE data first:
#' data <- closure_combine(
#'   mean = "3.5",
#'   sd = "2",
#'   n = 52,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Summary table:
#' closure_summarize(data)


closure_summarize <- function(data) {

  if (inherits(data$results, "closure_combine")) {
    data <- closure_pivot_longer(data)
  } else if (inherits(data$results, "closure_pivot_longer")) {
    check_closure_pivot_longer_unaltered(data)
  } else {
    abort_not_closure_data(allow_pivot = TRUE)
  }

  # Group by scale value, transforming the data into a list of data frames.
  # Mapping `nrow()` to the list returns the number of rows in each data frame,
  # i.e., the number of times that the scale values appear in the combinations.
  f_absolute <- data$results %>%
    split(data$results$value) %>%
    vapply(
      FUN       = nrow,
      FUN.VALUE = integer(1),
      USE.NAMES = TRUE
    )

  # Extract the scale values found in the combinations. Then, remove them from
  # their source, `f_absolute`, as they are no longer needed.
  value <- as.integer(names(f_absolute))
  names(f_absolute) <- NULL

  # Compute more statistics on the basis of the absolute frequencies.
  # median <- f_absolute / 2 + 0.5
  f_relative <- f_absolute / sum(f_absolute)

  # (The median can be tested like this:) all(f_absolute / 2 + 0.5 ==
  # vapply(f_absolute, function(x) median(1:x), double(1)))

  # Reconstruct the complete vector of possible scale values as a sequence from
  # scale minimum to scale maximum.
  value_completed <- data$scale_min:data$scale_max

  # If each possible value is instantiated in the values that were found in the
  # combinations, the results are complete and will be returned here. If not,
  # the zero counts of the uninstantiated values must be added to `value`, and
  # their zero frequencies to `f_absolute` and `f_relative`. This is what the
  # rest of the function will then do.
  if (length(value) == length(value_completed)) {
    return(add_class(
      tibble::tibble(
        value,
        f_absolute,
        f_relative
      ),
      "closure_summarize"
    ))
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

  add_class(
    tibble::tibble(
      value      = value_completed,
      f_absolute = f_absolute_completed,
      f_relative = f_relative_completed
    ),
    "closure_summarize"
  )

}
