write_basic <- function(data, path, technique) {
  check_generator_output(data, technique)

  check_value(path, "character")

  # Translate "." to the user's working directory. If the path was manually
  # given, `trimws()` removes leading or trailing whitespace, e.g., linebreaks.
  path <- if (path == ".") getwd() else trimws(path)

  # Refuse to rewrite data that were already saved to disk
  if (
    has_reading_class(data$inputs) &&
      any(names(data) == "directory") &&
      any(names(data$directory) == "path")
  ) {
    cli::cli_abort(
      c(
        "Results were already saved to disk.",
        "x" = "Folder with {technique} results is (or was) present at:",
        "x" = data$directory$path
      )
    )
  }

  if (
    !any(names(data) == "results") ||
      !identical(names(data$results), c("id", "sample", "horns"))
  ) {
    cli::cli_abort(
      c(
        "{technique} list must include a full `results` tibble.",
        "!" = "Results include samples and horns index values."
      )
    )
  }

  slash <- .Platform$file.sep

  # Prepare the name of the new directory to which `data` will be written.
  # Create a string where all the inputs (mean, SD, etc.) are connected through
  # dashes. Then, replace the decimal periods by underscores.
  name_new_dir <- data$inputs |>
    paste(collapse = "-") |>
    gsub("\\.", "_", x = _)

  # Full path of the new directory, not just the name
  path_new_dir <- paste0(
    path,
    slash,
    name_new_dir,
    slash
  )

  create_results_folder(path_new_dir)

  data_names <- names(data)

  # Write the small tibbles: those other than the "results"
  for (name in data_names[data_names != "results"]) {
    nanoparquet::write_parquet(
      data[[name]],
      file = paste0(path_new_dir, name, ".parquet")
    )
  }

  # Write the horns values separately; they are stored as a column, not a tibble
  nanoparquet::write_parquet(
    data$results["horns"],
    file = paste0(path_new_dir, "horns.parquet")
  )

  # The samples are also a column, but before writing them, they need to be
  # transformed into the same format used for streaming results to disk
  data$results$sample |>
    as_wide_n_tibble() |>
    nanoparquet::write_parquet(
      file = paste0(path_new_dir, "samples.parquet")
    )

  # Create info.md and issue an alert. Leave an empty line before.
  message()
  write_final_info_md(path_new_dir, technique)

  path_new_dir
}


# # Example inputs:
# include <- "stats_only"
# samples_cap <- NULL

read_basic <- function(
  path,
  technique,
  include = c("stats_only", "stats_and_horns", "capped_error", "all"),
  samples_cap = NULL
) {
  include <- arg_match_in_export(include)

  check_type(path, "character")
  check_type(samples_cap, "double", allow_null = TRUE)

  # "CLOSURE" --> "closure" etc.
  lowtech <- tolower(technique)

  # Prevent errors of accidentally included spaces or line breaks by removing
  # any such pattern from the start or end of the path. Breaks in particular can
  # be insidious because double-clicking on a path in the console and then
  # copying it will include a spurious trailing line break. However, this would
  # be invisible to the user because it is not printed in an error message.
  path <- trimws(path)

  # Check whether the `samples` and `samples_cap` arguments are consistent -- if
  # the former has either of these two values, the latter must be specified.
  if (include == "capped_error" && is.null(samples_cap)) {
    cli::cli_abort(
      c(
        "If `include` is \"{include}\", `samples_cap` must be specified.",
        "i" = "Use `samples_cap` to state a threshold -- if there are \
        more than this many samples, there will be an error."
      )
    )
  }

  if (include != "capped_error" && !is.null(samples_cap)) {
    cli::cli_abort(
      c(
        "If `samples_cap` is specified, `include` must be \"capped_error\".",
        "x" = "`include` is: \"{include}\"",
        "x" = "`samples_cap` is: `{samples_cap}`"
      )
    )
  }

  if (!dir.exists(path)) {
    cli::cli_abort(
      c(
        "Must choose an existing folder.",
        "x" = "Chosen folder does not exist:",
        "x" = path
      )
    )
  }

  slash <- .Platform$file.sep

  name_dir <- path |>
    strsplit(slash) |>
    call_on(\(x) {
      vec <- x[[1]]
      vec[length(vec)]
    })

  files_actual <- dir(path)

  # See comment right below
  parquet_opts <- nanoparquet::parquet_options(
    class = c("tbl_df", "tbl")
  )

  # Function to read the small Parquet files into tibbles. Since tibbles usually
  # have the "tbl_df" class but `read_parquet()` does not currently add it
  # (although it does add "tbl"), tweak nanoparquet's options just to be sure.
  read_file <- function(name) {
    path |>
      paste0(slash, name, ".parquet") |>
      nanoparquet::read_parquet(options = parquet_opts)
  }

  # The following two component tibbles will be needed even with empty results
  inputs <- "inputs" |>
    read_file() |>
    add_class(c(
      paste0(lowtech, "_read_include_", include),
      paste0(lowtech, "_generate")
    ))

  directory <- tibble::new_tibble(
    list(path = path),
    nrow = 1L
  )

  # Error if the folder contains other files than those needed, or if it does
  # not contain all of those needed. A bespoke message is shown in each case.
  if (!setequal(files_actual, FILES_EXPECTED)) {
    sample_meta <- path |>
      paste0(slash, "sample.parquet") |>
      nanoparquet::read_parquet_metadata(options = parquet_opts)

    # Escape hatch for empty results
    if (sample_meta$file_meta_data$num_rows == 0) {
      out_empty <- c(
        list(inputs = inputs),

        inputs$scale_min |>
          create_empty_results(inputs$scale_max) |>
          lapply(tibble::as_tibble),

        list(directory = directory)
      )

      check_generator_output(out_empty, technique, allow_empty = TRUE)

      return(out_empty)
    }

    msg_files_expected <- sort(FILES_EXPECTED)
    files_actual <- sort(files_actual)

    offenders_missing <- setdiff(msg_files_expected, files_actual)
    offenders_not_needed <- setdiff(files_actual, msg_files_expected)

    msg_missing <- if (length(offenders_missing) == 0) {
      NULL
    } else {
      c("x" = "Missing files: {offenders_missing}")
    }

    msg_not_needed <- if (length(offenders_not_needed) == 0) {
      NULL
    } else {
      c("x" = "Unnecessary files: {offenders_not_needed}")
    }

    cli::cli_abort(
      c(
        "Folder must contain all correct files (and no others).",
        "!" = "Expected files: {msg_files_expected}",
        msg_missing,
        msg_not_needed
      )
    )
  }

  # Read all the small files into a list. At this point, `out` corresponds to
  # `include == "stats_only"` because all of the additions further below
  # correspond to other variants of `include`.
  out <- list(
    inputs = inputs,
    metrics_main = "metrics_main" |> read_file(),
    metrics_horns = "metrics_horns" |> read_file(),
    frequency = "frequency" |> read_file()
  )

  # Parse mean and SD from the folder name
  mean_sd_str <- name_dir |>
    strsplit("-") |>
    call_on(function(x) x[[1]][2:3]) |>
    gsub("_", "\\.", x = _)

  # Check that files read from disk are correct
  if (
    !near(as.numeric(mean_sd_str[1]), as.numeric(out$inputs$mean)) ||
      !near(as.numeric(mean_sd_str[2]), as.numeric(out$inputs$sd))
  ) {
    cli::cli_abort(
      "Mean and SD in inputs.csv must match those in the folder's name."
    )
  }

  tryCatch(
    {
      out$inputs$mean <- mean_sd_str[1]
      out$inputs$sd <- mean_sd_str[2]
    },
    error = function(e) {
      cli::cli_abort("\"inputs\" must have \"mean\" and \"sd\" columns.")
    }
  )

  tryCatch(
    {
      out$frequency$value <- as.integer(out$frequency$value)
    },
    error = function(e) {
      cli::cli_abort("\"frequency\" must have a \"value\" column.")
    }
  )

  n_samples_all <- out$metrics_main$samples_all
  path_horns <- paste0(path, slash, "horns.parquet")

  # Adjudicate which additional parts of the results to read from disk, if any
  if (include == "stats_and_horns") {
    out$results <- tibble::new_tibble(
      x = list(
        id = as.double(seq_len(n_samples_all)),
        horns = nanoparquet::read_parquet(path_horns)[[1]]
      ),
      nrow = n_samples_all
    )
  } else if (include == "capped_error" && n_samples_all > samples_cap) {
    cli::cli_abort(
      c(
        "Number of samples exceeds the cap.",
        "x" = "`samples_cap` is: {samples_cap}",
        "x" = "Number of samples is: {n_samples_all}"
      )
    )
  } else if (include %in% c("all", "capped_error")) {
    # Read in the results separately. This requires transposing the samples via
    # `t()` because of the way they are stored on disk, which in turn is because
    # of the special requirements imposed by streaming in closure-core. Also,
    # read the horns values, add an ID column, and construct the final tibble.
    out$results <- tibble::new_tibble(
      x = list(
        # ID numbers (1 / 3)
        id = as.double(seq_len(n_samples_all)),

        # Result samples (2 / 3)
        sample = path |>
          paste0(slash, "samples.parquet") |>
          nanoparquet::read_parquet() |>
          t() |>
          tibble::as_tibble(.name_repair = "minimal") |>
          unclass() |>
          unname() |>
          tryCatch(
            error = function(e) {
              cli::cli_abort(
                c(
                  "Reading samples.parquet from disk failed.",
                  "x" = "Original error:",
                  "x" = "{e}",
                  "i" = "If memory is lacking, try\
                  `include = \"stats_and_horns\"`.\
                  In case even this takes up too much memory, use\
                  `include = \"stats_only\"`."
                ),
                call = rlang::caller_env(4)
              )
            }
          ),

        # Horns index values (3 / 3)
        horns = nanoparquet::read_parquet(path_horns)[[1]]
      ),
      nrow = n_samples_all
    )
  } else if (include != "stats_only") {
    cli::cli_abort("Internal error: invalid `include` variant \"{include}\"")
  }

  # Add a record of the folder's path
  out$directory <- tibble::new_tibble(
    list(path = path),
    nrow = 1L
  )

  # Final check -- is the reconstructed list correct?
  tryCatch(
    check_generator_output(out, technique),
    error = function(e) {
      cli::cli_abort(
        c(
          "Something went wrong when reading from disk.",
          "x" = "Original error:",
          "x" = "{e}"
        ),
        call = rlang::caller_env(4)
      )
    }
  )

  out
}
