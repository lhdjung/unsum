# Avoid NOTEs in R-CMD saying "no visible binding for global variable".
utils::globalVariables(c(".", "value", ".data"))


# Error if input is not an unchanged CLOSURE list.
check_closure_combine <- function(data) {
  top_level_is_correct <-
    is.list(data) &&
    length(data) == 4L &&
    identical(names(data), c("inputs", "metrics", "frequency", "results")) &&
    inherits(data$inputs, "closure_combine")

  if (!top_level_is_correct) {
    cli::cli_abort(
      message = c(
        "Input must be the output of `closure_combine()`.",
        "!" = "Such output is a list with the elements \
        \"inputs\", \"metrics\", \"frequency\", and \"results\"."
      ),
      call = rlang::caller_env()
    )
  }

  # Check the formats of the three tibbles that are elements of `data`, i.e., of
  # the output of `closure_combine()`:

  # Inputs (1 / 4)
  check_closure_combine_tibble(
    x = data$inputs,
    name = "inputs",
    dims = c(1L, 5L),
    col_names_types = list(
      "mean" = "character",
      "sd" = "character",
      "n" = c("integer", "double"),
      "scale_min" = c("integer", "double"),
      "scale_max" = c("integer", "double")
    )
  )

  # (Intermezzo to make sure that the assumption in the second check hold)
  check_scale(
    scale_min = data$inputs$scale_min,
    scale_max = data$inputs$scale_max,
    mean = data$inputs$mean,
    warning = "Don't change CLOSURE results before this step.",
    n = 2
  )

  # Metrics (2 / 4)
  check_closure_combine_tibble(
    x = data$metrics,
    name = "metrics",
    dims = c(1L, 5L),
    col_names_types = list(
      "combos_initial" = "integer",
      "combos_all" = "integer",
      "values_all" = "integer",
      "horns" = "double",
      "horns_uniform" = "double"
    )
  )

  # Frequency (3 / 4)
  check_closure_combine_tibble(
    x = data$frequency,
    name = "frequency",
    dims = c(data$inputs$scale_max - data$inputs$scale_min + 1, 4),
    col_names_types = list(
      "value" = "integer",
      "f_average" = "double",
      "f_absolute" = "integer",
      "f_relative" = "double"
    )
  )

  # Results (4 / 4)
  check_closure_combine_tibble(
    x = data$results,
    name = "results",
    dims = c(data$metrics$combos_all, 2L),
    col_names_types = list(
      "id" = "integer",
      "combination" = "list"
    )
  )

  # Additional checks:

  if (!is_seq_linear_basic(data$frequency$value)) {
    cli::cli_abort(
      message = c(
        "The `value` column in `frequency` must be a linear sequence.",
        "x" = "It is actually {data$frequency$value}."
      ),
      call = rlang::caller_env()
    )
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
    ) &&
      near(
        sum(data$frequency$f_absolute),
        0
      )
  )

  if (!f_relative_sums_up) {
    cli::cli_abort(
      message = c(
        "The `f_relative` column in `frequency` must sum up to 1 \
        (or 0, if `f_absolute` does).",
        "x" = "It actually sums up to {sum(data$frequency$f_relative)}."
      ),
      call = rlang::caller_env()
    )
  }

  all_results_integer <- data$results$combination |>
    vapply(
      FUN = function(x) typeof(x) == "integer",
      FUN.VALUE = logical(1)
    ) |>
    all()

  if (!all_results_integer) {
    cli::cli_abort("All `results` elements must be integer vectors.")
  }

  n <- data$inputs$n

  all_results_length_n <- data$results$combination |>
    vapply(
      FUN = function(x) length(x) == n,
      FUN.VALUE = logical(1)
    ) |>
    all()

  if (!all_results_length_n) {
    cli::cli_abort(
      message = "All `results` must have length `n` ({n}).",
      call = rlang::caller_env()
    )
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
    cli::cli_abort(
      message = c(
        "CLOSURE data must not be changed before passing them \
        to other `closure_*()` functions.",
        "!" = "Specifically, `{name}` must be a tibble with:",
        "*" = "{dims[1]} row{?s} and {dims[2]} column{?s}",
        "*" = "{this_these}: {cols_msg}"
      ),
      call = rlang::caller_env(2)
    )
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
check_scale <- function(
    scale_min,
    scale_max,
    mean = NULL,
    warning = NULL,
    n = 1
) {
  if (scale_min > scale_max) {
    cli::cli_abort(
      message = c(
        "Scale minimum can't be greater than scale maximum.",
        "!" = warning,
        "x" = "`scale_min` is {scale_min}.",
        "x" = "`scale_max` is {scale_max}."
      ),
      call = rlang::caller_env(n)
    )
  }
  # Coercing mean and scale bounds to avoid a false-positive error
  if (!is.null(mean)) {
    if (as.numeric(mean) < as.numeric(scale_min)) {
      cli::cli_abort(
        message = c(
          "Mean can't be less than scale minimum.",
          "!" = warning,
          "x" = "`mean` is {mean}.",
          "x" = "`scale_min` is {scale_min}."
        ),
        call = rlang::caller_env(n)
      )
    }
    if (as.numeric(mean) > as.numeric(scale_max)) {
      cli::cli_abort(
        message = c(
          "Mean can't be greater than scale maximum.",
          "!" = warning,
          "x" = "`mean` is {mean}.",
          "x" = "`scale_max` is {scale_max}."
        ),
        call = rlang::caller_env(n)
      )
    }
  }
}


# Make sure a value has the right type (or one of multiple allowed types), has
# length 1, and is not `NA`. Multiple allowed types are often `c("double",
# "integer")` which allows any numeric value, but no values of any other types.
check_value <- function(x, type) {
  name <- deparse(substitute(x))
  check_type(x, type, n = 2, name = name)
  if (length(x) != 1L) {
    cli::cli_abort(
      message = c(
        "`{name}` must have length 1.",
        "x" = "It has length {length(x)}."
      ),
      call = rlang::caller_env()
    )
  }
  if (is.na(x)) {
    cli::cli_abort(
      message = "`{name}` can't be `NA`.",
      call = rlang::caller_env()
    )
  }
}


check_type <- function(x, t, n = 1, name = NULL) {
  if (any(typeof(x) == t)) {
    return(invisible(NULL))
  }
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }
  msg_type <- if (length(t) == 1L) {
    "be of type"
  } else {
    "be one of the types"
  }
  cli::cli_abort(
    message = c(
      `!` = "`{name}` must {msg_type} {t}.",
      x = "It is {typeof(x)}."
    ),
    call = rlang::caller_env(n)
  )
}


# This helper creates the `frequency` part of `closure_combine()`'s output.
summarize_frequencies <- function(results, scale_min, scale_max, combos_all) {
  # Flatten the list of integer vectors into a single integer vector, then
  # create a frequency table for the values in that vector.
  f_absolute <- results |>
    unlist(use.names = FALSE) |>
    table()

  # Extract the scale values found in the combinations. Then, remove them from
  # their source, `f_absolute`, as they are no longer needed.
  value <- as.integer(names(f_absolute))
  f_absolute <- as.integer(f_absolute)

  # Compute the share of each individual value in the sum of all values.
  f_relative <- f_absolute / sum(f_absolute)

  # Divide by the number of samples instead to get the average number of values
  # in each bin.
  f_average <- f_absolute / combos_all

  # Reconstruct the complete vector of possible scale values as a sequence from
  # scale minimum to scale maximum.
  value_completed <- scale_min:scale_max
  n_completed <- length(value_completed)

  # If each possible value is instantiated in the values that were found in the
  # combinations, the results are complete and will be returned here. If not,
  # the zero counts of the uninstantiated values must be added to `value`, and
  # their zero frequencies to `f_absolute` and `f_relative`. This is what the
  # rest of the function will then do.
  if (length(value) == n_completed) {
    return(
      tibble::new_tibble(
        x = list(
          value = value,
          f_average = f_average,
          f_absolute = f_absolute,
          f_relative = f_relative
        ),
        nrow = n_completed
      )
    )
  }

  # At which indices in the complete vector of possible values are those values
  # that were actually found?
  indices_found <- which(value_completed %in% value)

  # Construct full-length vectors where each value is zero
  f_average_completed <- double(n_completed)
  f_absolute_completed <- integer(n_completed)
  f_relative_completed <- double(n_completed)

  # Fill in the non-zero values where appropriate
  f_average_completed[indices_found] <- f_average
  f_absolute_completed[indices_found] <- f_absolute
  f_relative_completed[indices_found] <- f_relative

  tibble::new_tibble(
    x = list(
      value = value_completed,
      f_average = f_average_completed,
      f_absolute = f_absolute_completed,
      f_relative = f_relative_completed
    ),
    nrow = n_completed
  )
}


# Copied from `dplyr::near()`
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}
