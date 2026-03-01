# Note: don't define anything here other than `generate_from_mean_sd_n()`.
# During the package build process, this file is manually sourced one extra time
# to determine the arguments of its one function, and creating any other objects
# in the file would make this less reliable.

# Note: some helper functions called here can be found in the R/utils.R file.
# The most notable exception, `create_combinations()`, is in
# R/extendr-wrappers.R, but all it does is to call into Rust code in
# scr/rust/src/lib.rs which, in turn, accesses closure-core. The latter is a
# Rust crate (roughly analogous to an R package) that contains the actual
# implementation of CLOSURE:
# https://github.com/lhdjung/closure-core/blob/master/src/lib.rs

# # For interactive testing:
# mean <- "5.00"
# sd <- "2.78"
# n <- 30
# scale_min <- 1
# scale_max <- 8
# technique <- "CLOSURE"
# path <- "."
# stop_after <- NULL
# include <- "stats_and_horns"
# rounding = "up_or_down"
# threshold = 5
# ask_to_proceed <- TRUE
# rounding_error_mean <- NULL
# rounding_error_sd <- NULL

generate_from_mean_sd_n <- function(
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  items = 1,
  technique,
  path = NULL,
  stop_after = NULL,
  include = c("stats_and_horns", "stats_only", "all"),
  rounding = "up_or_down",
  threshold = 5,
  ask_to_proceed = TRUE,
  rounding_error_mean = NULL,
  rounding_error_sd = NULL
) {
  # Comprehensive checks make sure that each argument is of the right type, has
  # length 1, and is not `NA`.
  check_single(mean, "character")
  check_single(sd, "character")
  check_single(n, c("double", "integer"))
  check_single(scale_min, c("double", "integer"))
  check_single(scale_max, c("double", "integer"))
  check_single(path, "character", allow_null = TRUE)
  check_single(stop_after, c("double", "integer"), allow_null = TRUE)
  check_single(rounding, "character")
  check_single(threshold, c("double", "integer"))
  check_single(ask_to_proceed, "logical")
  check_single(items, c("double", "integer"), allow_null = TRUE)

  mean_num <- as.numeric(mean)
  sd_num <- as.numeric(sd)

  check_scale(scale_min, scale_max, mean_num)

  # SPRITE-specific validation
  if (technique == "SPRITE") {
    # Prevent overflow crashes - SPRITE with items > 1 is prone to overflow
    # Be very conservative and require stop_after in most cases
    if (items > 1 && is.null(stop_after)) {
      abort_in_export(
        "`stop_after` is required when using SPRITE with multi-item scales.",
        "x" = "Currently using: `items = {items}`",
        "i" = "Add `stop_after` to limit results and prevent overflow.",
        "i" = "For example: `stop_after = 1000` will stop after SPRITE found
        the first 1000 results.",
        "i" = "This is a known limitation of the current SPRITE implementation."
      )
    }
  }

  include <- arg_match_in_export(include)

  # Should results be directly loaded into memory rather than written to disk?
  in_memory_mode <- is.null(path)

  # TODO: Maybe take `scale_min` and `scale_max` into account; they might
  # further confine the results of `unround()`!

  # Reconstruct the min and max possible values of the unknown original number
  # that was later rounded to the reported mean and SD values. Subtract each
  # lower bound (i.e., each minimum) from the respective reported value to
  # compute the rounding error.
  mean_sd_unrounded <- roundwork::unround(
    x = c(mean, sd),
    rounding = rounding,
    threshold = threshold
  )

  if (is.null(rounding_error_mean)) {
    rounding_error_mean <- mean_num - mean_sd_unrounded$lower[1]
  }

  if (is.null(rounding_error_sd)) {
    rounding_error_sd <- sd_num - mean_sd_unrounded$lower[2]
  }

  # If files should be written to disk, prepare a new folder for them, write
  # info.md and inputs.parquet into it, and record the new path.
  if (in_memory_mode) {
    parquet_config <- NULL
    # Error if `include` was specified even though `path` was not
    if (include != "stats_and_horns") {
      abort_in_export(
        "`include` requires `path` to be specified.",
        "x" = "`include` is \"{include}\".",
        "x" = "`path` is `NULL`.",
        "i" = "The purpose of `include` is to choose which files \
          to read into R after writing them to a folder chosen via `path`.",
        "i" = "Specify `path` as a string that points to a folder \
          on your computer, or as \".\" for your current working directory."
      )
    }
  } else {
    # In writing mode:
    path_new_dir <- prepare_folder_mean_sd_n(
      inputs = list(
        technique = technique,
        mean = mean,
        sd = sd,
        n = n,
        scale_min = scale_min,
        scale_max = scale_max,
        rounding = rounding,
        threshold = threshold
      ),
      path = path
    )
    parquet_config <- list(
      file_path = path_new_dir,
      batch_size = 1000
    )
  }

  # Make an educated guess about the complexity, and hence the runtime duration
  complexity <- switch(
    technique,
    "CLOSURE" = closure_gauge_complexity(
      mean = mean_num,
      sd = sd_num,
      n = n,
      scale_min = scale_min,
      scale_max = scale_max
    ),
    0 # default case
  )

  # This might be set to `TRUE` below
  need_to_ask <- FALSE

  msg_wait <- if (complexity < 1) {
    NULL
  } else if (complexity < 2) {
    "Just a second..."
  } else if (complexity < 3) {
    "This could take a minute..."
  } else {
    # With very high complexity, the user may be asked whether to proceed. The
    # need to ask will then depend on the `ask_to_proceed` argument, but there
    # will be no prompt unless the setting is interactive.
    need_to_ask <- ask_to_proceed && interactive()
    "ATTENTION: Long runtime ahead!"
  }

  if (!is.null(msg_wait)) {
    if (need_to_ask) {
      cli::cli_alert_warning(paste(msg_wait, "Do you wish to proceed?"))

      # In memory mode, make sure the user knows there is also the option to
      # write large results to disk.
      if (in_memory_mode) {
        cli::cli_alert_info(paste0(
          "Consider safely writing results to disk by specifying `path` in `",
          caller_fn_name(),
          "()`."
        ))
        cli::cli_alert_info("You would still obtain summary statistics in R.")
      }

      selection <- utils::menu(
        choices = c("Yes, wait", "No, abort"),
        title = "Please enter 1 or 2:"
      )

      if (selection == 1L) {
        cli::cli_alert_info("Running {technique}, please wait...")
      } else {
        fn_name <- caller_fn_name()
        cli::cli_alert_info("Aborting {.fn {fn_name}}.")
        return(invisible(NULL))
      }
    } else {
      # If the user should not be asked, just issue the alert
      cli::cli_alert(msg_wait)
    }
  }

  # For CLOSURE, `items` defaults to 1; for SPRITE, it must be provided
  items_val <- if (is.null(items)) 1L else as.integer(items)

  # Compute CLOSURE samples by calling into pre-compiled Rust code.
  out <- create_combinations(
    mean = mean_num,
    sd = sd_num,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    technique = technique,
    rounding_error_mean = rounding_error_mean,
    rounding_error_sd = rounding_error_sd,
    items = items_val,
    restrict_exact = NULL,
    restrict_min = NULL,
    write = parquet_config,
    stop_after = stop_after
  )

  # Count samples found; the place of this number depends on the output type
  n_samples_all <- if (in_memory_mode) {
    length(out$results$sample)
  } else {
    out$total_combinations
  }

  # By default, raise a warning if no results were found. If some were found but
  # more had been requested via `stop_after`, raise a different warning.
  if (n_samples_all == 0) {
    # CLOSURE is exhaustive, other techniques might not be
    msg_after <- switch(
      technique,
      "CLOSURE" = c(
        "x" = "Summary statistics are internally inconsistent.",
        "x" = "These numbers cannot describe the same sample."
      ),
      NULL
    )
    cli::cli_warn(c(
      "{technique} found no samples compatible with these inputs.",
      msg_after
    ))
  } else if (!is.null(stop_after) && n_samples_all < stop_after) {
    cli::cli_warn(c(
      "{technique} found fewer samples than requested via `stop_after`.",
      "!" = "`stop_after` is: {stop_after}",
      "!" = "Samples found: {n_samples_all}"
    ))
  }

  # Assemble the summary statistics and return an S7 result object. In memory
  # mode (i.e., without writing to disk), this is done manually here. All
  # tibble elements use the low-level `new_tibble()` for performance and
  # consistency. The S7 class replaces the former S3 class on `data$inputs`.
  if (in_memory_mode) {
    inputs_tbl <- tibble::new_tibble(
      x = list(
        technique = technique,
        mean = mean,
        sd = sd,
        n = n,
        scale_min = scale_min,
        scale_max = scale_max,
        rounding = rounding,
        threshold = threshold
      ),
      nrow = 1L
    )

    metrics_main_tbl <- out$metrics_main |>
      as.list() |>
      tibble::new_tibble(nrow = 1L)

    metrics_horns_tbl <- out$metrics_horns |>
      as.list() |>
      tibble::new_tibble(nrow = 1L)

    frequency_tbl <- out$frequency |>
      as.list() |>
      tibble::new_tibble(nrow = nrow(out$frequency))

    results_tbl <- tibble::new_tibble(
      x = out$results,
      nrow = n_samples_all
    )

    # In memory mode (i.e., without writing to disk), a message about successful
    # completion is left to display after the rest of the function has finished.
    # Exception: no message is shown if no results were found.
    on.exit({
      if (n_samples_all != 0) {
        # Empty line before the alert
        message()
        cli::cli_alert_success("All {technique} results found")
      }
    })

    # Construct and return the appropriate S7 class
    make_result <- switch(
      technique,
      "CLOSURE" = ClosureResultFull,
      "SPRITE"  = SpriteResultFull,
      cli::cli_abort("Internal error: unsupported technique \"{technique}\".")
    )

    make_result(
      inputs        = inputs_tbl,
      metrics_main  = metrics_main_tbl,
      metrics_horns = metrics_horns_tbl,
      frequency     = frequency_tbl,
      results       = results_tbl
    )
  } else {
    # In writing mode, read the statistics -- and, optionally, results -- that
    # were just written to disk into R. Note that R never held them in memory
    # before this point; they were created on the Rust level. The `include`
    # argument controls which parts of the results are loaded. This is to
    # prevent out-of-memory errors and long read times due to large data.
    out <- switch(
      technique,
      "CLOSURE" = closure_read(path_new_dir, include = include),
      "SPRITE" = sprite_read(path_new_dir, include = include),

      # Error if `technique` is allowed but file reading is not supported yet
      cli::cli_abort(
        "Internal error: File reading not supported for {technique}."
      )
    )

    # Overwrite info.md which now contains instructions for importing the files,
    # etc.; and issue an alert. As both steps give confirmatory signals to the
    # user, this is only done after reading the data successfully.
    write_final_info_md(path_new_dir, technique)

    # Return the list
    out
  }
}
