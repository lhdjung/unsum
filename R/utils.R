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
    any(c(5L, 6L) == length(data)) &&
    any(
      names(data) %in%
        c(
          tibbles_all,
          c(tibbles_all, "directory"),
          c(tibbles_all[!tibbles_all == "results"], "directory")
        )
    ) &&
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

  is_reading_class <- data$inputs |>
    class() |>
    grepl("^closure_read_include_", x = _)

  # Data that were already written to disk and read back into R -- special case
  if (any(is_reading_class)) {
    reading_class <- class(data$inputs)[is_reading_class]
    reading_class <- sub("^closure_read_include_", "", reading_class)

    if (length(reading_class) > 1) {
      cli::cli_abort("Cannot handle manipulated S3 classes.")
    }

    if (!any(names(data) == "directory")) {
      cli::cli_abort("Cannot handle manipulated output; \"directory\" missing.")
    }

    check_component_tibble(
      x = data$directory,
      name = "directory",
      dims = c(1L, 1L),
      col_names_types = list(
        "path" = "character"
      )
    )

    # Contradictory data
    if (reading_class == "stats_only" && any(names(data) == "results")) {
      cli::cli_abort(
        message = c(
          "Cannot hold \"results\" tibble because reading function \
          was called with `include == \"stats_only\"`."
        )
      )
    }

    # Check for "results" tibble without a "sample" column
    if (reading_class == "stats_and_horns") {
      check_component_tibble(
        x = data$results,
        name = "results",
        dims = c(data$metrics_main$samples_all, 2L),
        col_names_types = list(
          "id" = "integer",
          "horns" = "double"
        )
      )
    }
  }

  # In case the "results" tibble was returned directly by `closure_generate()`
  # or by a reading function with a setting that makes for equivalent "results"
  if (!any(is_reading_class) || any(reading_class == "capped_error")) {
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
  }

  # Additional checks:

  # The relative frequencies must sum up to 1 or 0 per group. As there are 3
  # groups, they must sum up to 3 in total. If they sum up to 0, the absolute
  # frequencies must also sum up to 0: it only makes sense if no values at all
  # were found. These comparisons use `near()`, copied from dplyr, to account
  # for accidental floating-point inaccuracies.
  f_relative_sums_up <- near(
    sum(data$frequency$f_relative),
    3
  ) ||
    (near(
      sum(data$frequency$f_relative),
      0
    ) &&
      near(
        sum(data$frequency$f_absolute),
        0
      ))

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
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  name <- deparse(substitute(x))
  check_type(x, type, n = 2, name = name, allow_null = allow_null)

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


# Copied from `dplyr::near()`
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}


create_results_folder <- function(path, n = 1) {
  if (dir.exists(path)) {
    cli::cli_abort(
      message = c(
        "Name of new folder must not be taken.",
        "x" = "Folder already exists:",
        "x" = path
      ),
      call = rlang::caller_env(n)
    )
  }
  dir.create(path)
}


# Where `inputs` is of the form `list(mean, sd, n, scale_min, scale_max)`. It
# creates a folder named after these summary statistics and returns the path to
# that new folder. It also writes two files into it: a general info.txt that
# will be overwritten later, and an inputs.parquet file with the inputs.
prepare_folder_mean_sd_n <- function(inputs, path, technique) {
  slash <- .Platform$file.sep

  # Prepare the name of the new directory to which `data` will later be written.
  # Create a string where all the inputs (mean, SD, etc.) are connected through
  # dashes. Then, replace the decimal periods by underscores.
  name_new_dir <- inputs |>
    paste(collapse = "-") |>
    gsub("\\.", "_", x = _)

  # Prefix with the name of the technique to make the origin very clear
  name_new_dir <- paste0(technique, "-", name_new_dir)

  path_current <- if (path == ".") getwd() else path

  # Full path of the new directory, not just the name
  path_new_dir <- paste0(
    path_current,
    slash,
    name_new_dir
  )

  create_results_folder(path_new_dir, n = 3)

  path_info_txt <- paste0(path_new_dir, slash, "info.txt")

  # "CLOSURE" --> "closure" etc.
  lowtech <- tolower(technique)

  # Create an info.txt file -- empty for now
  file.create(path_info_txt)

  connection <- file(path_info_txt)

  # While the results are written, provide a message to that effect in info.txt
  write(
    x = paste0(
      "DO NOT CHANGE THIS FOLDER OR ITS CONTENTS.\n\n",
      "Results of the ",
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

  # Parquet file with the inputs
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


# Write the final version of info.txt in a results folder. In `*_generate()`,
# this overwrites the placeholder text from `prepare_folder_mean_sd_n()`; and in
# `*_write()`, it creates info.txt in the first place. Also, issue an alert.
write_final_info_txt <- function(path, technique) {
  # "CLOSURE" --> "closure" etc.
  lowtech <- tolower(technique)

  # Open a connection to info.txt via a path like path/to/your/info.txt
  connection <- path |>
    paste0(.Platform$file.sep, "info.txt") |>
    file()

  # Create or overwrite info.txt
  write(
    x = paste0(
      "This folder contains the results of ",
      technique,
      ", created by ",
      "the R package unsum (version ",
      utils::packageVersion("unsum"),
      ").\n\n",
      "To load a summary of these results into R, use:\n",
      "unsum::",
      lowtech,
      "_read(\"",
      path,
      "\")\n\n",
      "For options to load the results themselves, see ",
      "documentation for `",
      lowtech,
      "_read()` at:\n",
      "https://lhdjung.github.io/unsum/reference/",
      lowtech,
      "_write.html\n\n",
      "Use a different path if the folder was moved. ",
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


# Transform unsum's CLOSURE results into the "n"-column format of the CSV files
# made by closure-core's test harness or the original Python implementation.
# This is also the format in which `closure_write()` saves the Parquet files.
# If the `closure_generate()` output was assigned to `data`, call:
# `as_wide_n_tibble(data$results$sample)`
as_wide_n_tibble <- function(samples_all) {
  samples_all |>
    tibble::as_tibble(.name_repair = "minimal") |>
    t() |>
    tibble::as_tibble(.name_repair = function(x) paste0("n", seq_along(x)))
}


# Get the name of the calling function as a string. By default, this is the
# function immediately calling the one within which `caller_fn_name()` is
# called. Choose the next-higher function with `n = 2` etc; or the current
# function with `n = 0`.
caller_fn_name <- function(n = 1) {
  as.character(rlang::caller_call(n + 1)[[1L]])
}


has_reading_class <- function(inputs, include = NULL) {
  pattern <- paste0("^.*_read_include_", include)

  inputs |>
    class() |>
    grepl(pattern, x = _) |>
    any()
}
