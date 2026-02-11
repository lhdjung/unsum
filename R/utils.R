# Global configuration ----------------------------------------------------

#' @include constants.R
NULL

# Avoid NOTEs in R-CMD saying "no visible binding for global variable".
utils::globalVariables(c(".", "value", ".data", "group_frequency_table"))


# Checks ------------------------------------------------------------------

# Error if the input is not an unchanged list containing the results of a
# function such as `closure_generate()`. Empty result sets error by default.
check_generator_output <- function(
  data,
  technique,
  allow_empty = FALSE
) {
  # "CLOSURE" --> "closure" etc.
  lowtech <- tolower(technique)

  top_level_is_correct <-
    is.list(data) &&
    any(c(5L, 6L) == length(data)) &&
    list(names(data)) %in% TIBBLE_NAMES_POSSIBLE_FORMS &&
    inherits(data$inputs, paste0(lowtech, "_generate"))

  if (!top_level_is_correct) {
    # Demo plots are not based on generated samples, so they will inevitably
    # fail the current check and need an escape hatch like this
    if (technique == "DEMO") {
      return(invisible(NULL))
    }

    msg_tibble_names <- paste0("\"", TIBBLE_NAMES, "\"")

    abort_in_export(
      paste0(
        "Input must be the output of `",
        lowtech,
        "_generate()` or `",
        lowtech,
        "_read()`."
      ),
      "!" = "Such output is a list with the elements {msg_tibble_names}."
    )
  }

  # Check the formats of the 5 or 6 tibbles that are elements of `data`, i.e.,
  # of the output of a function like `closure_generate()`:

  # Inputs (1 / 5)
  check_component_tibble(
    x = data$inputs,
    dims = c(1L, 8L),
    technique = technique,
    col_names_types = list(
      "technique" = "character",
      "mean" = "character",
      "sd" = "character",
      "n" = c("integer", "double"),
      "scale_min" = c("integer", "double"),
      "scale_max" = c("integer", "double"),
      "rounding" = "character",
      "threshold" = c("integer", "double")
    )
  )

  # (Intermezzo to make sure that the assumptions in the second check hold)
  check_scale(
    scale_min = data$inputs$scale_min,
    scale_max = data$inputs$scale_max,
    mean = data$inputs$mean,
    warning = "Don't change {technique} results before this step."
  )

  # Main metrics (2 / 5)
  check_component_tibble(
    x = data$metrics_main,
    dims = c(1L, 2L),
    technique = technique,
    col_names_types = list(
      "samples_all" = "double",
      "values_all" = "double"
    )
  )

  # Horns metrics (3 / 5)
  check_component_tibble(
    x = data$metrics_horns,
    dims = c(1L, 9L),
    technique = technique,
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

  # Length of the scale implied by the inputs. In `data$frequency`, each
  # `samples` category will have this very length. Empty results will only have
  # one such category, "all".
  scale_length <- data$inputs$scale_max - data$inputs$scale_min + 1

  # (Intermezzo to check for empty results)
  if (
    !allow_empty &&
      scale_length == nrow(data$frequency) &&
      all(is.nan(data$frequency$f_average)) &&
      all(is.nan(data$frequency$f_relative))
  ) {
    cli::cli_abort(
      "Results are empty; there is nothing to process any further.",
      call = rlang::caller_env()
    )
  }

  # Frequency (4 / 5)
  check_component_tibble(
    x = data$frequency,
    dims = c(3 * scale_length, 5),
    technique = technique,
    col_names_types = list(
      "samples" = "character",
      "value" = "integer",
      "f_average" = "double",
      "f_absolute" = "double",
      "f_relative" = "double"
    )
  )

  # (Long intermezzo before the final tibble check)

  is_reading_class <- data$inputs |>
    class() |>
    grepl(paste0("^", lowtech, "_read_include_"), x = _)

  # Data that were already written to disk and read back into R -- special case
  if (any(is_reading_class)) {
    reading_class <- class(data$inputs)[is_reading_class]
    reading_class <- sub(
      pattern = paste0("^", lowtech, "_read_include_"),
      replacement = "",
      x = reading_class
    )

    if (length(reading_class) > 1) {
      cli::cli_abort("Cannot handle manipulated S3 classes.")
    }

    if (!any(names(data) == "directory")) {
      cli::cli_abort("Cannot handle manipulated output; \"directory\" missing.")
    }

    check_component_tibble(
      x = data$directory,
      dims = c(1L, 1L),
      technique = technique,
      col_names_types = list(
        "path" = "character"
      )
    )

    # Contradictory data
    if (reading_class == "stats_only" && any(names(data) == "results")) {
      cli::cli_abort(
        c(
          "Cannot hold \"results\" tibble because reading function \
          was called with `include == \"stats_only\"`."
        )
      )
    }

    # Check for "results" tibble without a "sample" column
    if (reading_class == "stats_and_horns") {
      check_component_tibble(
        x = data$results,
        dims = c(data$metrics_main$samples_all, 2L),
        technique = technique,
        col_names_types = list(
          "id" = "double",
          "horns" = "double"
        )
      )
    }
  }

  # In case the "results" tibble was returned directly by a function like
  # `closure_generate()` or by a reading function with a setting that makes for
  # equivalent "results"
  if (!any(is_reading_class) || any(reading_class == "capped_error")) {
    # Results (5 / 5)
    check_component_tibble(
      x = data$results,
      dims = c(data$metrics_main$samples_all, 3L),
      technique = technique,
      col_names_types = list(
        "id" = "double",
        "sample" = "list",
        "horns" = "double"
      )
    )
  }

  # Additional checks:

  # The relative frequencies must sum up to 1 or 0 per group. As there are 3
  # groups, they must sum up to 3 in total. If they sum up to 0, the absolute
  # frequencies must also sum up to 0: it only makes sense if no values at all
  # were found. These comparisons use `near()`, copied from dplyr, to account
  # for accidental floating-point inaccuracies.
  f_relative_sum <- sum(data$frequency$f_relative)
  freqs_sum_up <- near(f_relative_sum, 3) ||
    (near(f_relative_sum, 0) && near(sum(data$frequency$f_absolute), 0))

  if (!freqs_sum_up) {
    cli::cli_abort(
      c(
        "The `f_relative` column in `frequency` must sum up to 1 \
        (or 0, if `f_absolute` does).",
        "x" = "It actually sums up to {f_relative_sum}."
      ),
      call = rlang::caller_env()
    )
  }
}


# Check each element of the output of a function like `closure_generate()` for
# correct format.
check_component_tibble <- function(
  x,
  dims,
  technique,
  col_names_types,
  msg_main = NULL
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
    tibble_name <- x |>
      substitute() |>
      deparse() |>
      sub("^.*\\$", "", x = _)

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

    # "CLOSURE" --> "closure" etc.
    lowtech <- tolower(technique)

    if (is.null(msg_main)) {
      msg_main <- "{technique} data must not be changed before passing them \
        to other `{lowtech}_*()` functions."
    }

    abort_in_export(
      msg_main,
      "!" = "Specifically, `{tibble_name}` must be a tibble with:",
      "*" = "{dims[1]} row{?s} and {dims[2]} column{?s}",
      "*" = "{this_these}: {cols_msg}"
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
  warning = NULL
) {
  if (scale_min > scale_max) {
    abort_in_export(
      "Scale minimum can't be greater than scale maximum.",
      "!" = warning,
      "x" = "`scale_min` is {scale_min}.",
      "x" = "`scale_max` is {scale_max}."
    )
  }

  # Coercing mean and scale bounds to avoid a false-positive error
  if (!is.null(mean)) {
    if (as.numeric(mean) < as.numeric(scale_min)) {
      abort_in_export(
        "Mean can't be less than scale minimum.",
        "!" = warning,
        "x" = "`mean` is {mean}.",
        "x" = "`scale_min` is {scale_min}."
      )
    }
    if (as.numeric(mean) > as.numeric(scale_max)) {
      abort_in_export(
        "Mean can't be greater than scale maximum.",
        "!" = warning,
        "x" = "`mean` is {mean}.",
        "x" = "`scale_max` is {scale_max}."
      )
    }
  }
}


# Make sure a value has the right type (or one of multiple allowed types), has
# length 1, and is not `NA`. Multiple allowed types are often `c("double",
# "integer")` which allows any numeric value, but no values of any other types.
check_value <- function(x, type, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  name <- deparse(substitute(x))
  check_type(x, type, n = 2, name = name, allow_null = allow_null)

  if (length(x) != 1L) {
    abort_in_export(
      "`{name}` must have length 1.",
      "x" = "It has length {length(x)}."
    )
  }

  if (is.na(x)) {
    abort_in_export("`{name}` can't be `NA`.")
  }
}


# Make sure a value has the correct type (or is `NULL`, if allowed)
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

  abort_in_export(
    "!" = "`{name}` must {msg_type} {t}.",
    "x" = "It is {typeof(x)}."
  )
}


# Adapted from scrutiny
check_whole_number <- function(
  x,
  tolerance = .Machine$double.eps^0.5,
  n = 1,
  name = NULL
) {
  # Stop before the error message if `x` is a whole number
  if (near(x, floor(x), tol = tolerance)) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  abort_in_export(
    "`{name}` must be a whole number (integer or double).",
    "x" = "It is actually: {x}"
  )
}


# Make sure a value has the correct length (or is `NULL`, if allowed)
check_length <- function(x, l, n = 1, name = NULL, allow_null = FALSE) {
  if (length(x) == l || (allow_null && is.null(x))) {
    return(invisible(NULL))
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  abort_in_export(
    "!" = "`{name}` must have length {l}.",
    "x" = "It has length {length(x)}."
  )
}


# A vector of frequencies will sum up to 1 or 0 if the frequencies are relative,
# or it will consist of all-integerish absolute frequencies. The expected
# relative sum can be adjusted for vectors with multiple whole groups.
check_frequency_vector <- function(x, sum_relative = 1) {
  sum_x <- sum(x)

  if (
    near(sum_x, sum_relative) ||
      near(sum_x, 0) ||
      all(is_whole_number(x))
  ) {
    return(invisible(NULL))
  }

  abort_in_export(
    "`freqs` must be a vector of frequencies (relative or absolute)."
  )
}


# General helpers ---------------------------------------------------------

# Pipe helper that allows for calling primitives, anonymous functions, and
# function factories within a pipe workflow. As a toy example: `object |>
# call_on(function(x) x[x > 10])`
call_on <- function(.x, .f, ...) {
  .f(.x, ...)
}


# Copied from `dplyr::near()`
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}


# Adapted from scrutiny
is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - floor(x)) < tol
}


# Add an S3 class to an object. Using the prefix form which is more efficient.
add_class <- function(x, new_class) {
  `class<-`(x, c(new_class, class(x)))
}


# Get the name of the calling function as a string. By default, this is the
# function immediately calling the one within which `caller_fn_name()` is
# called. Choose the next-higher function with `n = 2` etc; or the current
# function with `n = 0`.
caller_fn_name <- function(n = 1) {
  as.character(rlang::caller_call(n + 1)[[1L]])
}


# Throw an error that names the top-level exported, user-called function as the
# source; e.g., "Error in `exported_fn()`", not "Error in `internal_helper()`".
abort_in_export <- function(...) {
  cli::cli_abort(
    message = c(...),
    call = caller_env_last_export(),
    .envir = parent.frame()
  )
}


# Similar to `abort_in_export()` but as an alternative to `match.arg()` /
# `rlang::arg_match()`. Advantage: If this function throws an error, it will
# name the exported function called by the user as the source of the problem.
# Arguments shared with `rlang::arg_match()` work as there.
arg_match_in_export <- function(arg, values = NULL, multiple = FALSE) {
  arg_expr <- rlang::enexpr(arg)
  arg_name <- as.character(arg_expr)

  # If values not provided, extract from the calling function's formals
  if (is.null(values)) {
    parent_formals <- formals(sys.function(sys.parent()))

    if (!arg_name %in% names(parent_formals)) {
      cli::cli_abort(
        "Internal error: {.arg {arg_name}} not found in calling function."
      )
    }

    values <- eval(parent_formals[[arg_name]], envir = parent.frame())
  }

  val <- rlang::eval_bare(arg_expr, env = rlang::caller_env())

  # Early escape hatch for performance in the typical use case
  if (!multiple && is.character(val) && length(val) == 1L && val %in% values) {
    return(val)
  }

  # This is retained for non-typical cases and for its characteristic error msg
  rlang::arg_match(
    arg = val,
    values = values,
    multiple = multiple,
    error_arg = arg_name,
    error_call = caller_env_last_export()
  )
}


# Find the environment of the exported function at the top of the call stack,
# i.e., the last or outermost exported function that was called. This is useful
# as a helper within `abort_in_export()` so that the function that was called by
# the user is named in the error message as the site where the error occurred.
caller_env_last_export <- function(package_name = NULL) {
  # If `package_name` is not provided, try detect it in the package environment
  if (is.null(package_name)) {
    pkg_env <- parent.env(environment())
    if (isNamespace(pkg_env)) {
      package_name <- getNamespaceName(pkg_env)
    } else {
      cli::cli_abort(c(
        "Could not determine package name.",
        "i" = "Please provide the `package_name` argument."
      ))
    }
  }

  # Get all frames and calls on the call stack
  frames <- sys.frames()
  calls <- sys.calls()

  # Get the namespace of the package
  ns <- tryCatch(
    getNamespace(package_name),
    error = function(e) {
      cli::cli_abort(
        "Package \"{package_name}\" not found or not loaded.",
        "x" = "Original error:",
        "x" = "{e}"
      )
    }
  )

  # Get list of exported functions from the package
  exports <- getNamespaceExports(ns)

  # Find the last (most recent in call stack) exported function
  for (i in seq_along(calls)) {
    fn <- calls[[i]][[1]]

    # Get name of the current function. If not found, skip to the next element.
    if (is.name(fn)) {
      fn_name <- as.character(fn)
    } else if (is.call(fn)) {
      fn_name <- deparse(fn)[1]
    } else {
      next
    }

    # Remove any package prefix (e.g., "pkg::fn" --> "fn")
    name_bare <- sub("^.*::", "", fn_name)

    # When an exported function is found, return the environment of its frame
    if (name_bare %in% exports) {
      return(frames[[i]])
    }
  }

  cli::cli_warn("No exported function found on the call stack.")
  environment()
}


# Specific logic ----------------------------------------------------------

# Folder into which CLOSURE-type results will be written, with a helpful check
create_results_folder <- function(path) {
  if (dir.exists(path)) {
    abort_in_export(
      "Name of new folder must not be taken.",
      "x" = "Folder already exists:",
      path
    )
  }

  dir.create(path)
}


# Where `inputs` is of the form `list(mean, sd, n, scale_min, scale_max)`. It
# creates a folder named after these summary statistics and returns the path to
# that new folder. It also writes two files into it: a general info.md that will
# be overwritten later, and an inputs.parquet file with the inputs.
prepare_folder_mean_sd_n <- function(inputs, path) {
  slash <- .Platform$file.sep

  # Prepare the name of the new directory to which `data` will later be written.
  # Create a string where all the inputs (mean, SD, etc.) are connected through
  # dashes. Then, replace the decimal periods by underscores.
  name_new_dir <- inputs |>
    paste(collapse = "-") |>
    gsub("\\.", "_", x = _)

  technique <- inputs$technique

  path_current <- if (path == ".") getwd() else path

  # Full path of the new directory, not just the name
  path_new_dir <- paste0(
    path_current,
    slash,
    name_new_dir
  )

  create_results_folder(path_new_dir)

  path_info_md <- paste0(path_new_dir, slash, "info.md")

  # "CLOSURE" --> "closure" etc.
  lowtech <- tolower(technique)

  # Create an info.md file -- empty for now
  file.create(path_info_md)

  connection <- file(path_info_md)

  # While the results are written, provide a message to that effect in info.md
  write(
    x = paste0(
      "# DO NOT CHANGE THIS FOLDER OR ITS CONTENTS.\n\nResults of the ",
      technique,
      " technique are currently being written ",
      "to the results.parquet file (unless the process was interrupted). ",
      "This message will be overwritten once the process has finished.\n\n",
      "For more information, visit:\n",
      "https://lhdjung.github.io/unsum/reference/",
      lowtech,
      "_generate.html"
    ),
    file = connection
  )

  close(connection)

  # Write a Parquet file with the inputs
  inputs |>
    tibble::new_tibble(
      nrow = 1L,
      class = paste0(lowtech, "_generate")
    ) |>
    nanoparquet::write_parquet(
      file = paste0(path_new_dir, slash, "inputs.parquet")
    )

  # Return the path of the new folder
  path_new_dir
}


# Write the final version of info.md in a results folder. In `*_generate()`,
# this overwrites the placeholder text from `prepare_folder_mean_sd_n()`; and in
# `*_write()`, it creates info.md in the first place. Also, issue an alert.
write_final_info_md <- function(path, technique) {
  # "CLOSURE" --> "closure" etc.
  lowtech <- tolower(technique)

  # Open a connection to info.md via a path like path/to/your/info.md
  connection <- path |>
    paste0(.Platform$file.sep, "info.md") |>
    file()

  # Create or overwrite info.md
  write(
    x = paste0(
      "This folder contains the results of ",
      technique,
      ", created by the R package unsum (version ",
      utils::packageVersion("unsum"),
      ").\n\nTo load a summary of these results into R, use:\nunsum::",
      lowtech,
      "_read(\"",
      path,
      "\")\n\n",
      "For options to load the results themselves, see documentation for `",
      lowtech,
      "_read()` at:\nhttps://lhdjung.github.io/unsum/reference/",
      lowtech,
      "_write.html\n\nUse a different path if the folder was moved. ",
      "In any case, opening the files will require a Parquet reader. ",
      "For more information, visit:\n",
      "https://lhdjung.github.io/unsum/reference/",
      lowtech,
      "_generate.html"
    ),
    file = connection
  )

  close(connection)

  cli::cli_alert_success("All {technique} files written to:\n{path}")
}


# Transform unsum's CLOSURE result lists into the "n"-column format in which
# `closure_generate()` streams Parquet files to disk (if `path` is specified),
# and in which `closure_write()` saves Parquet files. This is also the format of
# the CSV files made by closure-core's test harness or the original Python
# implementation. Optimized for performance, not readability. As an example,
# assuming that `data` is `closure_generate()` output, call:
# `as_wide_n_tibble(data$results$sample)`
as_wide_n_tibble <- function(samples_all) {
  n_samples <- length(samples_all)
  n_final_cols <- length(samples_all[[1]])

  # Use the numbers from 1 to `n_samples` to name the elements of `samples_all`.
  # Then, turn the list into a tibble, which is necessary to transpose it using
  # `t()`. Finally, turn the result into a tibble again, but this time, name the
  # columns like "n1", "n2", etc.
  `names<-`(samples_all, seq_len(n_samples)) |>
    tibble::new_tibble(nrow = n_final_cols) |>
    t() |>
    tibble::as_tibble(.name_repair = function(x) paste0("n", seq_along(x)))
}


# Run this on a `data$inputs` tibble to check whether `data` was already written
# to disk before. If so, this will have been marked by an (invisible) S3 class.
has_reading_class <- function(inputs, include = NULL) {
  pattern <- paste0("^.*_read_include_", include)

  # Search the `inputs` classes for one like "closure_read_include_stats_only"
  inputs |>
    class() |>
    grepl(pattern, x = _) |>
    any()
}
