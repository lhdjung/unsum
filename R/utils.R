# Avoid NOTEs in R-CMD saying "no visible binding for global variable".
utils::globalVariables(c(".", "value", ".data", "group_frequency_table"))

# Load helpers that are only needed at build-time. Storing them under inst/
# rather than R/ avoids unnecessarily including them in the final binary.
source("inst/build-helpers/fn-formals.R")


# Error if input is not an unchanged CLOSURE list.
check_closure_generate <- function(data) {
  tibbles_all <- c(
    "inputs",
    "metrics_main",
    "metrics_horns",
    "frequency",
    "results"
  )

  top_level_is_correct <-
    is.list(data) &&
    length(data) == 5L &&
    identical(names(data), tibbles_all) &&
    inherits(data$inputs, "closure_generate")

  if (!top_level_is_correct) {
    msg_tibbles_all <- paste0("\"", tibbles_all, "\"")
    cli::cli_abort(
      message = c(
        "Input must be the output of `closure_generate()` \
        or `closure_read()`.",
        "!" = "Such output is a list with the elements \
        {msg_tibbles_all}."
      ),
      call = rlang::caller_env()
    )
  }

  # Check the formats of the three tibbles that are elements of `data`, i.e., of
  # the output of `closure_generate()`:

  # Inputs (1 / 5)
  check_component_tibble(
    x = data$inputs,
    name = "inputs",
    dims = c(1L, 7L),
    col_names_types = list(
      "mean" = "character",
      "sd" = "character",
      "n" = c("integer", "double"),
      "scale_min" = c("integer", "double"),
      "scale_max" = c("integer", "double"),
      "rounding" = c("character"),
      "threshold" = c("integer", "double")
    )
  )

  # (Intermezzo to make sure that the assumptions in the second check hold)
  check_scale(
    scale_min = data$inputs$scale_min,
    scale_max = data$inputs$scale_max,
    mean = data$inputs$mean,
    warning = "Don't change CLOSURE results before this step.",
    n = 2
  )

  # Main metrics (2 / 5)
  check_component_tibble(
    x = data$metrics_main,
    name = "metrics_main",
    dims = c(1L, 3L),
    col_names_types = list(
      "samples_initial" = "integer",
      "samples_all" = "double",
      "values_all" = "double"
    )
  )

  # Horns metrics (3 / 5)
  check_component_tibble(
    x = data$metrics_horns,
    name = "metrics_horns",
    dims = c(1L, 9L),
    col_names_types = list(
      "mean" = "double",
      "uniform" = "double",
      "sd" = "double",
      "cv" = "double",
      "mad" = "double",
      "min" = "double",
      "median" = "double",
      "max" = "double",
      "range" = "double"
    )
  )

  # Frequency (4 / 5)
  check_component_tibble(
    x = data$frequency,
    name = "frequency",
    dims = c(
      3 * (data$inputs$scale_max - data$inputs$scale_min + 1),
      5
    ),
    col_names_types = list(
      "samples" = "character",
      "value" = "integer",
      "f_average" = "double",
      "f_absolute" = "double",
      "f_relative" = "double"
    )
  )

  # Results (5 / 5)
  check_component_tibble(
    x = data$results,
    name = "results",
    dims = c(data$metrics_main$samples_all, 3L),
    col_names_types = list(
      "id" = "integer",
      "sample" = "list",
      "horns" = "double"
    )
  )

  # Additional checks:

  # The relative frequencies must sum up to 1 or 0 per group. As there are 3
  # groups, they must sum up to 3 in total. If they sum up to 0, the absolute
  # frequencies must also sum up to 0: it only makes sense if no values at all
  # were found. These comparisons use `near()`, copied from dplyr, to account
  # for accidental floating-point inaccuracies.
  f_relative_sums_up <- near(
    sum(data$frequency$f_relative),
    3
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
    cli::cli_abort(
      message = c(
        "The `f_relative` column in `frequency` must sum up to 1 \
        (or 0, if `f_absolute` does).",
        "x" = "It actually sums up to {sum(data$frequency$f_relative)}."
      ),
      call = rlang::caller_env()
    )
  }

  all_results_integer <- data$results$sample |>
    vapply(
      FUN = function(x) typeof(x) == "integer",
      FUN.VALUE = logical(1)
    ) |>
    all()

  if (!all_results_integer) {
    cli::cli_abort("All `results` elements must be integer vectors.")
  }

  n <- data$inputs$n

  all_results_length_n <- data$results$sample |>
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


check_closure_horns_analyze <- function(data) {
  msg_analyze_output <- "Need output of `closure_horns_analyze()`."

  check_component_tibble(
    x = data$closure_generate_inputs,
    name = "closure_generate_inputs",
    dims = c(1L, 7L),
    col_names_types = list(
      "mean" = c("character"),
      "sd" = c("character"),
      "n" = c("integer", "double"),
      "scale_min" = c("integer", "double"),
      "scale_max" = c("integer", "double"),
      "rounding" = c("character"),
      "threshold" = c("integer", "double")
    ),
    msg_main = msg_analyze_output,
    n = 2
  )

  check_component_tibble(
    x = data$horns_metrics,
    name = "horns_metrics",
    dims = c(1L, 9L),
    col_names_types = list(
      "mean" = c("integer", "double"),
      "uniform" = c("integer", "double"),
      "sd" = c("integer", "double"),
      "cv" = c("integer", "double"),
      "mad" = c("integer", "double"),
      "min" = c("integer", "double"),
      "median" = c("integer", "double"),
      "max" = c("integer", "double"),
      "range" = c("integer", "double")
    ),
    msg_main = msg_analyze_output,
    n = 2
  )

  scale_min <- data$closure_generate_inputs$scale_min
  scale_max <- data$closure_generate_inputs$scale_max

  check_component_tibble(
    x = data$frequency_horns_min,
    name = "frequency_horns_min",
    dims = c(scale_max - scale_min + 1L, 4L),
    col_names_types = list(
      "value" = "integer",
      "f_average" = "double",
      "f_absolute" = "integer",
      "f_relative" = "double"
    ),
    msg_main = msg_analyze_output,
    n = 2
  )

  check_component_tibble(
    x = data$frequency_horns_max,
    name = "frequency_horns_max",
    dims = c(scale_max - scale_min + 1L, 4L),
    col_names_types = list(
      "value" = "integer",
      "f_average" = "double",
      "f_absolute" = "integer",
      "f_relative" = "double"
    ),
    msg_main = msg_analyze_output,
    n = 2
  )

  check_component_tibble(
    x = data$results,
    name = "results",
    dims = c(nrow(data$results), 3L),
    col_names_types = list(
      "id" = "integer",
      "horns" = "double"
    ),
    msg_main = msg_analyze_output,
    n = 2
  )
}


# Check each element of `closure_generate()` for correct format.
check_component_tibble <- function(
  x,
  name,
  dims,
  col_names_types,
  msg_main = NULL,
  n = 2
) {
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
    if (is.null(msg_main)) {
      msg_main <- "CLOSURE data must not be changed before passing them \
        to other `closure_*()` functions."
    }
    cli::cli_abort(
      message = c(
        msg_main,
        "!" = "Specifically, `{name}` must be a tibble with:",
        "*" = "{dims[1]} row{?s} and {dims[2]} column{?s}",
        "*" = "{this_these}: {cols_msg}"
      ),
      call = rlang::caller_env(n)
    )
  }
}


# Functions like `closure_generate()` that take `scale_min` and `scale_max`
# arguments need to make sure that min <= max. Functions that take the mean into
# account also need to check that it is within these bounds. Such functions
# include `closure_generate()` but not `closure_count_initial()`.
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
check_value <- function(x, type, allow_null = FALSE) {
  name <- deparse(substitute(x))
  check_type(x, type, n = 2, name = name, allow_null = allow_null)

  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

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


check_type <- function(x, t, n = 1, name = NULL, allow_null = FALSE) {
  if (
    any(t == typeof(x)) ||
    (allow_null && is.null(x)) ||
    (is.integer(x) && any(t == "double"))
  ) {
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
  if (length(t) == 1 && t == "double") {
    t <- "double or integer"
  }
  cli::cli_abort(
    message = c(
      `!` = "`{name}` must {msg_type} {t}.",
      x = "It is {typeof(x)}."
    ),
    call = rlang::caller_env(n)
  )
}


check_length <- function(x, l, n = 1, name = NULL, allow_null = FALSE) {
  if (length(x) == l || (allow_null && is.null(x))) {
    return(invisible(NULL))
  }
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }
  cli::cli_abort(
    message = c(
      `!` = "`{name}` must have length {l}.",
      x = "It has length {length(x)}."
    ),
    call = rlang::caller_env(n)
  )
}


# This helper creates the `frequency` part of `closure_generate()`'s output.
summarize_frequencies <- function(results, scale_min, scale_max, samples_all) {
  # Flatten the list of integer vectors into a single integer vector, then
  # create a frequency table for the values in that vector.
  f_absolute <- results |>
    unlist(use.names = FALSE) |>
    table()

  # Extract the scale values found in the samples. Then, remove them from
  # their source, `f_absolute`, as they are no longer needed.
  value <- as.integer(names(f_absolute))
  f_absolute <- as.integer(f_absolute)

  # Compute the share of each individual value in the sum of all values.
  f_relative <- f_absolute / sum(f_absolute)

  # Divide by the number of samples instead to get the average number of values
  # in each bin.
  f_average <- f_absolute / samples_all

  # Reconstruct the complete vector of possible scale values as a sequence from
  # scale minimum to scale maximum.
  value_completed <- scale_min:scale_max
  n_completed <- length(value_completed)

  # If each possible value is instantiated in the values that were found in the
  # samples, the results are complete and will be returned here. If not,
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


# Where `inputs` is of the form `list(mean, sd, n, scale_min, scale_max)`. It
# creates a folder named after these summary statistics and returns the path to
# that new folder. It also writes two files into it: a general info.txt that
# will be overwritten later, and an inputs.parquet file with the inputs.
write_mean_sd_n_folder <- function(inputs, path) {
  slash <- .Platform$file.sep

  # Prepare the name of the new directory to which `data` will later be written.
  # Create a string where all the inputs (mean, SD, etc.) are connected through
  # dashes. Then, replace the decimal periods by underscores.
  name_new_dir <- inputs |>
    paste(collapse = "-") |>
    gsub("\\.", "_", x = _)

  # Prefix with the name of the technique to make the origin very clear
  name_new_dir <- paste0("CLOSURE-", name_new_dir)

  path_current <- if (path == ".") getwd() else path

  # Full path of the new directory, not just the name
  path_new_dir <- paste0(
    path_current,
    slash,
    name_new_dir
  )

  if (dir.exists(path_new_dir)) {
    cli::cli_abort(
      message = c(
        "Name of new folder must not be taken.",
        "x" = "Folder already exists:",
        "x" = "{path_new_dir}"
      ),
      call = rlang::caller_env()
    )
  }

  path_info_txt <- paste0(path_new_dir, slash, "info.txt")

  dir.create(path_new_dir)
  file.create(path_info_txt)

  # While the results are written, provide a message to that effect in info.txt
  connection <- file(path_info_txt)
  write(
    x = paste0(
      "DO NOT CHANGE THIS FOLDER OR ITS CONTENTS.\n\n",
      "Results of the CLOSURE technique are currently being written ",
      "to the results.parquet file (unless the process was interrupted). ",
      "This message will be overwritten once the process has finished.\n\n",
      "For more information, visit:\n",
      "https://lhdjung.github.io/unsum/reference/closure_generate.html"
    ),
    file = connection
  )
  close(connection)

  # Parquet file with the inputs
  nanoparquet::write_parquet(
    tibble::new_tibble(
      x = inputs,
      nrow = 1L,
      class = "closure_generate"
    ),
    file = paste0(path_new_dir, slash, "inputs.parquet")
  )

  # Return the path of the new folder
  path_new_dir
}


# After writing results to disk, the info.txt file should be updated
overwrite_info_txt <- function(path) {
  slash <- .Platform$file.sep

  path_info_txt <- paste0(path, slash, "info.txt")

  # Overwrite text in info.txt -- it has been a placeholder saying that CLOSURE
  # results are currently being written. Now it says writing them has finished.
  connection <- file(path_info_txt)
  write(
    x = paste0(
      "This folder contains the results of CLOSURE, created by ",
      "the R package unsum.\n\n",
      "To load these files into R, use:\n",
      "unsum::closure_read(\"", path, "\")\n\n",
      "Use a different path if the folder was moved. ",
      "In any case, opening the files will require a Parquet reader.\n\n",
      "For more information, visit:\n",
      "https://lhdjung.github.io/unsum/reference/closure_generate.html"
    ),
    file = connection
  )
  close(connection)

  cli::cli_alert_success(paste0("All files written to:\n", path))
}


# Transform unsum's CLOSURE results into the "n"-column format of the CSV files
# made by closure-core's test harness or the original Python implementation.
# This is also the format in which `closure_write()` saves the Parquet files.
as_wide_n_tibble <- function(samples_all) {
  samples_all |>
    tibble::as_tibble(.name_repair = "minimal") |>
    t() |>
    tibble::as_tibble(.name_repair = function(x) paste0("n", seq_along(x)))
}


# This is the reverse operation of `as_wide_n_tibble()` except it also
# constructs a full "results" tibble, as in `closure_generate()`'s output.
as_results_tibble <- function(n_cols) {
  n_samples_all <- nrow(n_cols)
  tibble::new_tibble(
    x = list(
      id = seq_len(n_samples_all),
      sample = n_cols |>
        t() |>
        tibble::as_tibble(.name_repair = "minimal") |>
        as.list() |>
        unname()
    ),
    nrow = n_samples_all
  )
}

