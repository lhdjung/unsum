#' @include classes.R
NULL

write_basic <- function(data, path, technique) {
  check_generator_output(data, technique)

  check_single(path, "character")

  # Translate "." to the user's working directory. If the path was manually
  # given, `trimws()` removes leading or trailing whitespace, e.g., linebreaks.
  path <- path_sanitize(path)

  # Refuse to rewrite data that were already saved to disk
  if (S7::S7_inherits(data, ClosureResultFromDisk) || S7::S7_inherits(data, SpriteResultFromDisk)) {
    abort_in_export(
      "Results were already saved to disk.",
      "x" = "Folder with {technique} results is (or was) present at:",
      "x" = data@directory$path
    )
  }

  # Only ClosureResultFull / SpriteResultFull carry a full results tibble with
  # samples. Any other subclass lacks samples and cannot be written this way.
  if (!S7::S7_inherits(data, ClosureResultFull) && !S7::S7_inherits(data, SpriteResultFull)) {
    abort_in_export(
      "{technique} list must include a full `results` tibble.",
      "!" = "Results include samples and horns index values."
    )
  }

  slash <- .Platform$file.sep

  # Prepare the name of the new directory to which `data` will be written.
  # Create a string where all the inputs (mean, SD, etc.) are connected through
  # dashes. Then, replace the decimal periods by underscores.
  name_new_dir <- data@inputs |>
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

  # Write the four summary tibbles
  for (name in c("inputs", "metrics_main", "metrics_horns", "frequency")) {
    nanoparquet::write_parquet(
      S7::prop(data, name),
      file = paste0(path_new_dir, name, ".parquet")
    )
  }

  # Write the horns values separately; they are stored as a column, not a tibble
  nanoparquet::write_parquet(
    data@results["horns"],
    file = paste0(path_new_dir, "horns.parquet")
  )

  # The samples are also a column, but before writing them, they need to be
  # transformed into the same format used for streaming results to disk
  data@results$sample |>
    as_wide_n_tibble() |>
    nanoparquet::write_parquet(
      file = paste0(path_new_dir, "sample.parquet")
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

  # Prevent errors of accidentally included spaces or line breaks by removing
  # any such pattern from the start or end of the path. Breaks in particular can
  # be insidious because double-clicking on a path in the console and then
  # copying it will include a spurious trailing line break. However, this would
  # be invisible to the user because it is not printed in an error message.
  path <- trimws(path)

  # Check whether the `samples` and `samples_cap` arguments are consistent -- if
  # the former has either of these two values, the latter must be specified.
  if (include == "capped_error" && is.null(samples_cap)) {
    abort_in_export(
      "If `include` is \"{include}\", `samples_cap` must be specified.",
      "i" = "Use `samples_cap` to state a threshold -- if there are \
        more than this many samples, there will be an error."
    )
  }

  if (include != "capped_error" && !is.null(samples_cap)) {
    abort_in_export(
      "If `samples_cap` is specified, `include` must be \"capped_error\".",
      "x" = "`include` is: \"{include}\"",
      "x" = "`samples_cap` is: `{samples_cap}`"
    )
  }

  if (!dir.exists(path)) {
    abort_in_export(
      "Must choose an existing folder.",
      "x" = "Chosen folder does not exist:",
      "x" = path
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

  # The following two component tibbles will be needed even with empty results.
  # With S7, the class membership replaces the former S3 class strings that
  # used to be attached to `inputs`.
  inputs <- "inputs" |>
    read_file()

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

    # Escape hatch for empty results: construct a stats-only S7 object with
    # stub metrics/frequency generated by the Rust-level helper
    if (sample_meta$file_meta_data$num_rows == 0) {
      empty_parts <- inputs$scale_min |>
        create_empty_results(inputs$scale_max) |>
        lapply(tibble::as_tibble)

      make_stats_only <- switch(
        technique,
        "CLOSURE" = ClosureResultStatsOnly,
        "SPRITE"  = SpriteResultStatsOnly,
        cli::cli_abort("Internal error: unsupported technique \"{technique}\".")
      )

      out_empty <- tryCatch(
        make_stats_only(
          inputs        = inputs,
          metrics_main  = empty_parts$metrics_main,
          metrics_horns = empty_parts$metrics_horns,
          frequency     = empty_parts$frequency,
          directory     = directory
        ),
        error = function(e) {
          abort_in_export(
            "Something went wrong when reading empty results from disk.",
            "x" = "Original error:",
            "x" = "{e}"
          )
        }
      )

      return(out_empty)
    }

    # Error path
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

    abort_in_export(
      "Folder must contain all correct files (and no others).",
      "!" = "Expected files: {msg_files_expected}",
      msg_missing,
      msg_not_needed
    )
  }

  # Read the four summary tibbles. The technique class is determined by the
  # `technique` parameter (passed from `closure_read()` / `sprite_read()`).
  metrics_main_tbl  <- "metrics_main"  |> read_file()
  metrics_horns_tbl <- "metrics_horns" |> read_file()
  frequency_tbl     <- "frequency"     |> read_file()

  # Parse mean and SD from the folder name
  mean_sd_str <- name_dir |>
    strsplit("-") |>
    call_on(function(x) x[[1]][2:3]) |>
    gsub("_", "\\.", x = _)

  # Check that files read from disk are correct
  if (
    !near(as.numeric(mean_sd_str[1]), as.numeric(inputs$mean)) ||
      !near(as.numeric(mean_sd_str[2]), as.numeric(inputs$sd))
  ) {
    abort_in_export(
      "Mean and SD in inputs.parquet must match those in the folder's name."
    )
  }

  tryCatch(
    {
      inputs$mean <- mean_sd_str[1]
      inputs$sd   <- mean_sd_str[2]
    },
    error = function(e) {
      abort_in_export("\"inputs\" must have \"mean\" and \"sd\" columns.")
    }
  )

  tryCatch(
    {
      frequency_tbl$value <- as.integer(frequency_tbl$value)
    },
    error = function(e) {
      abort_in_export("\"frequency\" must have a \"value\" column.")
    }
  )

  n_samples_all <- metrics_main_tbl$samples_all
  path_horns    <- paste0(path, slash, "horns.parquet")

  # Helper: construct the right pair of S7 classes based on technique
  make_from_disk <- switch(
    technique,
    "CLOSURE" = list(
      stats_only      = ClosureResultStatsOnly,
      stats_and_horns = ClosureResultStatsAndHorns,
      all             = ClosureResultAll
    ),
    "SPRITE" = list(
      stats_only      = SpriteResultStatsOnly,
      stats_and_horns = SpriteResultStatsAndHorns,
      all             = SpriteResultAll
    ),
    cli::cli_abort("Internal error: unsupported technique \"{technique}\".")
  )

  # Shared base arguments for all from-disk S7 constructors
  base_args <- list(
    inputs        = inputs,
    metrics_main  = metrics_main_tbl,
    metrics_horns = metrics_horns_tbl,
    frequency     = frequency_tbl,
    directory     = directory
  )

  # Adjudicate which additional parts of the results to read from disk, if any.
  # The user-facing capped_error check runs before the structural tryCatch so
  # that its message is not obscured by "Something went wrong when reading."
  if (include == "capped_error" && n_samples_all > samples_cap) {
    abort_in_export(
      "Number of samples exceeds the cap.",
      "x" = "`samples_cap` is: {samples_cap}",
      "x" = "Number of samples is: {n_samples_all}"
    )
  }

  # Build optional `results_tbl`, then construct and return the S7 object.
  # Wrap S7 construction in tryCatch to give a clean error if the data on disk
  # turns out to be inconsistent or otherwise unexpected.
  results_tbl <- if (include == "stats_and_horns") {
    tibble::new_tibble(
      x = list(
        id    = as.double(seq_len(n_samples_all)),
        horns = nanoparquet::read_parquet(path_horns)[[1]]
      ),
      nrow = n_samples_all
    )
  } else if (include %in% c("all", "capped_error")) {
    # Read in the results separately. This requires transposing the samples via
    # `t()` because of the way they are stored on disk, which in turn is because
    # of the special requirements imposed by streaming in closure-core.
    sample_col <- path |>
      paste0(slash, "sample.parquet") |>
      nanoparquet::read_parquet() |>
      t() |>
      tibble::as_tibble(.name_repair = "minimal") |>
      unclass() |>
      unname() |>
      tryCatch(
        error = function(e) {
          abort_in_export(
            "Reading sample.parquet from disk failed.",
            "x" = "Original error:",
            "x" = "{e}",
            "i" = "If memory is lacking, try \
              `include = \"stats_and_horns\"`. \
              In case even this takes up too much memory, use \
              `include = \"stats_only\"`."
          )
        }
      )

    tibble::new_tibble(
      x = list(
        id     = as.double(seq_len(n_samples_all)),
        sample = sample_col,
        horns  = nanoparquet::read_parquet(path_horns)[[1]]
      ),
      nrow = n_samples_all
    )
  } else if (include == "stats_only") {
    NULL
  } else {
    cli::cli_abort("Internal error: invalid `include` variant \"{include}\"")
  }

  constructor_args <- if (is.null(results_tbl)) {
    base_args
  } else {
    c(base_args, list(results = results_tbl))
  }

  constructor <- if (is.null(results_tbl)) {
    make_from_disk$stats_only
  } else if (include == "stats_and_horns") {
    make_from_disk$stats_and_horns
  } else {
    make_from_disk$all
  }

  tryCatch(
    do.call(constructor, constructor_args),
    error = function(e) {
      abort_in_export(
        "Something went wrong when reading from disk.",
        "x" = "Original error:",
        "x" = "{e}"
      )
    }
  )
}
