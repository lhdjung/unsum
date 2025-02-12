
# Avoid NOTEs in R-CMD saying "no visible binding for global variable".
utils::globalVariables(c(".", "value"))


# Add S3 class(es) to an object `x`
add_class <- function (x, new_class) {
  `class<-`(x, value = c(new_class, class(x)))
}

# Reading CLOSURE data from disk can lead to spurious differences in attributes;
# specifically, in pointers. However, what matters when comparing two data
# frames produced by different CLOSURE implementations is only the values, not
# any transitory details about the way R stores them in memory.

# Test whether two objects are identical except for their attributes. In other
# words, when the attributes are removed, is the rest identical between the two?
identical_except_attributes <- function(x, y) {
  identical(
    x = `attributes<-`(x, NULL),
    y = `attributes<-`(y, NULL)
  )
}


# Given two data frames, does each pair of columns contain the same values? This
# is tested by sorting the columns first, then comparing them. CLOSURE results
# are hard to predict, and different correct implementations can lead to
# differently sorted columns. The mark of correctness, then, is whether the
# columns are identical after being ordered equally. This assumes the same
# number and names of columns. With `message = TRUE`, it will print which column
# pair is the first unequal one if the result is `FALSE`.
identical_sorted_cols <- function(x, y, message = FALSE) {
  if (ncol(x) != ncol(y)) {
    cli::cli_abort("Different numbers of columns.")
  }
  if (!identical(colnames(x), colnames(y))) {
    cli::cli_abort("Different column names.")
  }
  for (n in seq_len(ncol(x))) {
    if (!identical(sort(x[[n]]), sort(y[[n]]))) {
      if (message) {
        message(paste("Different at", n))
      }
      return(FALSE)
    }
  }
  TRUE
}


# Error if input is not an unchanged CLOSURE list.
check_closure_combine <- function(data) {

  top_level_is_correct <-
    is.list(data) &&
    length(data) == 3L &&
    identical(names(data), c("metadata", "frequency", "results")) &&
    inherits(data$metadata, "closure_combine")

  if (!top_level_is_correct) {
    cli::cli_abort(c(
      "Input must be the output of `closure_combine()`.",
      "!" = "Such output is a list with the elements \\
      \"metadata\", \"frequency\", and \"results\"."
    ))
  }

  # Check the formats of the three tibbles that are elements of `data`, i.e., of
  # the output of `closure_combine()`:

  # Metadata (1 / 3)
  check_closure_combine_tibble(
    x = data$metadata,
    name = "metadata",
    dims = c(1L, 8L),
    col_names_types = list(
      "mean" = "character",
      "sd" = "character",
      "n" = c("integer", "double"),
      "scale_min" = c("integer", "double"),
      "scale_max" = c("integer", "double"),
      "combos_initial" = "integer",
      "combos_all" = "integer",
      "values_all" = "integer"
    )
  )

  # Frequency (2 / 3)
  check_closure_combine_tibble(
    x = data$frequency,
    name = "frequency",
    dims = c(data$metadata$scale_max - data$metadata$scale_min + 1, 3),
    col_names_types = list(
      "value" = "integer",
      "f_absolute" = "integer",
      "f_relative" = "double"
    )
  )

  # Results (3 / 3)
  check_closure_combine_tibble(
    x = data$results,
    name = "results",
    dims = c(data$metadata$combos_all, 2L),
    col_names_types = list(
      "id" = "integer",
      "combination" = "list"
    )
  )


  # Additional checks:

  check_scale(
    scale_min = data$metadata$scale_min,
    scale_max = data$metadata$scale_max,
    mean      = data$metadata$mean
  )

  if (!is_seq_linear_basic(data$frequency$value)) {
    cli::cli_abort(c(
      "The `value` column in `frequency` must be a linear sequence.",
      "x" = "It is actually {data$frequency$value}."
    ))
  }

  # The relative frequencies must sum up to 1 or 0. In the latter case, the
  # absolute frequencies must also sum up to 0: it only makes sense if no values
  # at all were found. These comparisons use `near()`, copied from dplyr, to
  # account for accidental floating-point inaccuracies.
  f_relative_sums_up <- near(
    sum(data$frequency$f_relative),
    1
  ) || (
    near(
      sum(data$frequency$f_relative),
      0
    ) && near(
      sum(data$frequency$f_absolute),
      0
    )
  )

  if (!f_relative_sums_up) {
    cli::cli_abort(c(
      "The `f_relative` column in `frequency` must sum up to 1 \\
      (or 0, if `f_absolute` does).",
      "x" = "It actually sums up to {sum(data$frequency$f_relative)}."
    ))
  }

  all_results_integer <- data$results$combination %>%
    vapply(
      FUN = function(x) typeof(x) == "integer",
      FUN.VALUE = logical(1)
    ) %>%
    all()

  if (!all_results_integer) {
    cli::cli_abort("All `results` elements must be integer vectors.")
  }

  n <- data$metadata$n

  all_results_length_n <- data$results$combination %>%
    vapply(
      FUN = function(x) length(x) == n,
      FUN.VALUE = logical(1)
    ) %>%
    all()

  if (!all_results_length_n) {
    cli::cli_abort("All `results` must have length `n` ({n}).")
  }

}


# Check each element of `closure_combine()` for correct format.
check_closure_combine_tibble <- function(x, name, dims, col_names_types) {

  tibble_is_correct <-
    inherits(x, "tbl_df") &&
    all(dim(x) == dims) &&
    identical(names(x), names(col_names_types)) &&
    all(
      mapply(
        function(a, b) any(a == b),
        vapply(x, typeof, character(1)),
        col_names_types
      )
    )

  if (!tibble_is_correct) {
    cols_msg <- paste0(
      "\"",
      names(col_names_types),
      "\" (",
      unname(col_names_types),
      ")"
    )
    this_these <- if (length(col_names_types) == 1L) {
      "This column name and type"
    } else {
      "These column names and types"
    }
    cli::cli_abort(c(
      "CLOSURE data must not be changed before passing them \\
      to other `closure_*()` functions.",
      "!" = "Specifically, `{name}` must be a tibble with:",
      "*" = "{dims[1]} row{?s} and {dims[2]} column{?s}",
      "*" = "{this_these}: {cols_msg}"
    ))
  }

}


# Borrowed from scrutiny's internals and used within `check_closure_combine()`,
# this checks whether a vector is a linear sequence (1, 2, 3) or not (3, 1, 7).
is_seq_linear_basic <- function(x) {
  if (length(x) < 3L) {
    return(TRUE)
  }
  diff_first <- x[2L] - x[1L]
  for (i in 3L:length(x)) {
    if (x[i] - x[i - 1L] != diff_first) {
      return(FALSE)
    }
  }
  TRUE
}


# Functions like `closure_combine()` that take `scale_min` and `scale_max`
# arguments need to make sure that min <= max. Functions that take the mean into
# account also need to check that it is within these bounds. Such functions
# include `closure_combine()` but not `closure_count_initial()`.
check_scale <- function(scale_min, scale_max, mean = NULL) {
  if (scale_min > scale_max) {
    cli::cli_abort(c(
      "Scale minimum can't be greater than scale maximum.",
      "x" = "`scale_min` is {scale_min}.",
      "x" = "`scale_max` is {scale_max}."
    ))
  }
  # Coercing mean and scale bounds to avoid a false-positive error
  if (!is.null(mean)) {
    if (as.numeric(mean) < as.numeric(scale_min)) {
      cli::cli_abort(c(
        "Mean can't be less than scale minimum.",
        "x" = "`mean` is {mean}.",
        "x" = "`scale_min` is {scale_min}."
      ))
    }
    if (as.numeric(mean) > as.numeric(scale_max)) {
      cli::cli_abort(c(
        "Mean can't be greater than scale maximum.",
        "x" = "`mean` is {mean}.",
        "x" = "`scale_max` is {scale_max}."
      ))
    }
  }
}


# Make sure a value has the right type (or one of multiple allowed types), has
# length 1, and is not `NA`. Multiple allowed types are often `c("double",
# "integer")` which allows any numeric value, but no values of any other types.
check_value <- function(x, type) {
  if (!any(type == typeof(x))) {
    name <- deparse(substitute(x))
    type_intro <- if (length(type) == 1) {
      "be of type"
    } else {
      "have one of the types"
    }
    cli::cli_abort(c(
      "`{name}` must {type_intro} {type}.",
      "x" = "It is {typeof(x)}."
    ))
  }
  if (length(x) != 1L) {
    name <- deparse(substitute(x))
    cli::cli_abort(c(
      "`{name}` must have length 1.",
      "x" = "It has length {length(x)}."
    ))
  }
  if (is.na(x)) {
    name <- deparse(substitute(x))
    cli::cli_abort("`{name}` can't be `NA`.")
  }
}


# This helper creates the `frequency` part of `closure_combine()`'s output.
summarize_frequencies <- function(results, scale_min, scale_max) {

  # Flatten the list of integer vectors into a single integer vector, then
  # create a frequency table for the values in that vector.
  f_absolute <- results %>%
    unlist(use.names = FALSE) %>%
    table()

  # Extract the scale values found in the combinations. Then, remove them from
  # their source, `f_absolute`, as they are no longer needed.
  value <- as.integer(names(f_absolute))
  f_absolute <- as.integer(f_absolute)

  # Compute the share of each individual value in the sum of all values.
  f_relative <- f_absolute / sum(f_absolute)

  # Reconstruct the complete vector of possible scale values as a sequence from
  # scale minimum to scale maximum.
  value_completed <- scale_min:scale_max

  # If each possible value is instantiated in the values that were found in the
  # combinations, the results are complete and will be returned here. If not,
  # the zero counts of the uninstantiated values must be added to `value`, and
  # their zero frequencies to `f_absolute` and `f_relative`. This is what the
  # rest of the function will then do.
  if (length(value) == length(value_completed)) {
    return(tibble::tibble(
      value,
      f_absolute,
      f_relative
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

  tibble::tibble(
    value      = value_completed,
    f_absolute = f_absolute_completed,
    f_relative = f_relative_completed
  )

}


# Copied from `dplyr::near()`
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}


