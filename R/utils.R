# Global configuration ----------------------------------------------------

#' @include constants.R standalone-last-export.R
NULL

# Avoid NOTEs in R-CMD saying "no visible binding for global variable".
# fmt: skip
utils::globalVariables(c(
  ".", "value", ".data", "group_frequency_table",
  "formals_final", "path", "include", "samples_cap", "data", "frequency"
))


# Register S7 methods when the package is loaded; see
# https://rconsortium.github.io/S7/articles/packages.html
.onLoad <- function(...) {
  S7::methods_register()
}

# Enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL


# Build mode ---------------------------------------------------------------

#' Switch Rust build to debug mode
#'
#' Modifies `tools/config.R` so that `devtools::load_all()` produces a debug
#' (unoptimized) build. Debug builds compile faster but run much slower.
#'
#' @return Invisible `NULL`, called for side effect.
#' @noRd
use_debug <- function() {
  switch_build_mode(debug = TRUE)
}

#' Switch Rust build to release mode
#'
#' Modifies `tools/config.R` so that `devtools::load_all()` produces a release
#' (optimized) build. Release builds compile slower but run much faster.
#'
#' @return Invisible `NULL`, called for side effect.
#' @noRd
use_release <- function() {
  switch_build_mode(debug = FALSE)
}

switch_build_mode <- function(debug) {
  root <- rprojroot::find_package_root_file()
  mode_label <- if (debug) "debug" else "release"

  # 1. Update tools/config.R (persists the setting for configure / R CMD INSTALL)
  config_path <- file.path(root, "tools", "config.R")
  txt <- readLines(config_path)

  target <- if (debug) "is_debug <- TRUE" else "is_debug <- FALSE"
  pattern <- "^is_debug <- (TRUE|FALSE)$"
  idx <- grep(pattern, txt)

  if (length(idx) != 1L) {
    cli::cli_abort(
      "Expected exactly one {.code is_debug <- ...} line in
      {.file tools/config.R}, found {length(idx)}."
    )
  }

  config_already_set <- trimws(txt[idx]) == target

  if (!config_already_set) {
    txt[idx] <- target
    writeLines(txt, config_path)
  }

  # 2. Re-source config.R to regenerate src/Makevars immediately.
  #    devtools::load_all() does not always re-run configure, so
  #    we must regenerate Makevars ourselves.
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  source("tools/config.R", local = TRUE)

  # 3. Remove the compiled shared object to force recompilation
  pkg <- sub("^package:", "", search()[[2]])

  for (ext in c(".so", ".dll")) {
    f <- file.path(root, "src", paste0(pkg, ext))
    if (file.exists(f)) file.remove(f)
  }

  if (config_already_set) {
    cli::cli_alert_info("Build mode is already set to {.strong {mode_label}}.")
  } else {
    cli::cli_alert_success("Switched build mode to {.strong {mode_label}}.")
  }

  message()
  cli::cli_alert_info("Run {.code devtools::load_all()} to recompile.")
  invisible(NULL)
}


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

  # S7 result objects have already passed property type validation at
  # construction time; skip the structural list check for them.
  if (!S7::S7_inherits(data, ResultListFromMeanSdN)) {
    top_level_is_correct <-
      is.list(data) &&
      any(c(6L, 7L, 9L, 10L) == length(data)) &&
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
        "Input must be the output of `{lowtech}_generate()` or
        `{lowtech}_read()`.",
        "i" = "Such output is a list with the elements {msg_tibble_names}."
      )
    }
  }

  # Check the formats of the 5 or 6 tibbles that are elements of `data`, i.e.,
  # of the output of a function like `closure_generate()`:

  # Inputs (1 / 6)
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

  # Main metrics (2 / 6)
  check_component_tibble(
    x = data$metrics_main,
    dims = c(1L, 2L),
    technique = technique,
    col_names_types = list(
      "samples_all" = "double",
      "values_all" = "double"
    )
  )

  # Horns metrics (3 / 6)
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
      all(is.nan(data$frequency$f_count)) &&
      all(is.nan(data$frequency$f_relative))
  ) {
    abort_in_export(
      "Results are empty; there is nothing to process any further."
    )
  }

  # Frequency (4 / 6)
  check_component_tibble(
    x = data$frequency,
    dims = c(3 * scale_length, 4),
    technique = technique,
    col_names_types = list(
      "samples" = "character",
      "value" = "integer",
      "f_count" = "double",
      "f_relative" = "double"
    )
  )

  # Frequency distribution (5 / 6)
  check_component_tibble(
    x = data$frequency_dist,
    # Dimensions can't be checked because the correct dimensions of this
    # particular tibble can't be inferred from any other statistics
    dims = NULL,
    technique = technique,
    col_names_types = list(
      "value" = "integer",
      "count" = "integer",
      "n_samples" = "integer"
    )
  )

  # Modality tibbles — only present for in-memory results (not disk reads)
  if (
    !S7::S7_inherits(data, ResultListFromMeanSdN) &&
      any(names(data) == "modality_counts")
  ) {
    check_component_tibble(
      x = data$modality_counts,
      dims = c(scale_length, 3L),
      technique = technique,
      col_names_types = list(
        "value" = "integer",
        "count_lo" = "integer",
        "count_hi" = "integer"
      )
    )

    check_component_tibble(
      x = data$modality_pairs,
      dims = c(scale_length - 1L, 4L),
      technique = technique,
      col_names_types = list(
        "value_a" = "integer",
        "value_b" = "integer",
        "resolved" = "logical",
        "a_greater" = "logical"
      )
    )

    check_component_tibble(
      x = data$modality_conclusion,
      dims = c(1L, 4L),
      technique = technique,
      col_names_types = list(
        "can_be_unimodal" = "logical",
        "can_be_bimodal" = "logical",
        "j_shape_low" = "logical",
        "j_shape_high" = "logical"
      )
    )
  }

  # (Long intermezzo before the final tibble check)

  is_reading_class <- data$inputs |>
    class() |>
    grepl(paste0("^", lowtech, "_read_include_"), x = _)

  reading_class_exists <- any(is_reading_class)

  # Data that were already written to disk and read back into R -- special case
  if (reading_class_exists) {
    reading_class <- class(data$inputs)[is_reading_class]
    reading_class <- sub(
      pattern = paste0("^", lowtech, "_read_include_"),
      replacement = "",
      x = reading_class
    )

    if (length(reading_class) > 1) {
      abort_in_export("Cannot handle modified S3 classes.")
    }

    if (!any(names(data) == "directory")) {
      abort_in_export("Cannot handle modified output; \"directory\" missing.")
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
      abort_in_export(
        "Cannot hold \"results\" tibble because reading function \
        was called with `include == \"stats_only\"`."
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
  if (!reading_class_exists || any(reading_class == "capped_error")) {
    # Results (6 / 6)
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
  # for accidental floating-point inaccuracies. Empty results *can* be allowed.
  f_sum_relative <- sum(data$frequency$f_relative)
  freqs_sum_up <- near(f_sum_relative, 3)

  # Need `isTRUE()` because `freqs_sum_up` can be `NA` but the condition must
  # still be met
  if (!isTRUE(freqs_sum_up)) {
    f_sum_count <- sum(data$frequency$f_count)
    data_is_empty <- is.nan(f_sum_relative) && near(f_sum_count, 0)

    # Empty data might be allowed, depending on the caller
    if (data_is_empty && allow_empty) {
      return(invisible(NULL))
    } else if (data_is_empty) {
      abort_in_export("Empty results are not allowed in this function.")
    }

    msg_actual_sum <- if (is.nan(f_sum_relative)) {
      NULL
    } else {
      c("x" = "It actually sums up to {f_sum_relative}.")
    }

    abort_in_export(
      "The `f_relative` column in `frequency` must sum up to 1 \
        (or 0, if `f_count` does).",
      msg_actual_sum
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
    (is.null(dims) || all(dim(x) == dims)) &&
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

    if (is.null(dims)) {
      abort_in_export(msg_main)
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
check_single <- function(x, type, allow_null = FALSE) {
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


# Check whether an S7 object has a specific property, named as a string.
has_property <- function(x, name) {
  tryCatch(!is.null(S7::prop(x, name)), error = function(e) FALSE)
}


# Specific logic ----------------------------------------------------------

# Translate "." to the user's working directory. If the path was manually given,
# `trimws()` removes leading or trailing whitespace, e.g., linebreaks.
path_sanitize <- function(path) {
  out <- switch(path, "." = getwd(), trimws(path))

  # Error if `path` was given as a period with whitespace, as empty, or as
  # whitespace-only (which was trimmed above)
  if (out %in% c("", ".")) {
    msg_main <- if (out == ".") {
      "In `path`, whitespace around the period is not allowed."
    } else if (path == "") {
      "`path` cannot be \"\", an empty string."
    } else {
      "`path` cannot consist of whitespace."
    }

    abort_in_export(
      msg_main,
      "i" = "Did you mean \".\" for your current working directory?"
    )
  }

  out
}


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
