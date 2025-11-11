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
  check_value(mean, "character")
  check_value(sd, "character")
  check_value(n, c("double", "integer"))
  check_value(scale_min, c("double", "integer"))
  check_value(scale_max, c("double", "integer"))
  check_value(path, "character", allow_null = TRUE)
  check_value(stop_after, c("double", "integer"), allow_null = TRUE)
  check_value(rounding, "character")
  check_value(threshold, c("double", "integer"))
  check_value(ask_to_proceed, "logical")

  mean_num <- as.numeric(mean)
  sd_num <- as.numeric(sd)

  check_scale(scale_min, scale_max, mean_num, n = 2)

  include <- rlang::arg_match(include)

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
  # info.txt and inputs.parquet into it, and record the new path.
  if (in_memory_mode) {
    parquet_config <- NULL
    # Error if `include` was specified even though `path` was not
    if (include != "stats_and_horns") {
      cli::cli_abort(
        message = c(
          "`include` requires `path` to be specified.",
          "x" = "`include` is \"{include}\".",
          "x" = "`path` is `NULL`.",
          "i" = "The purpose of `include` is to choose which files \
          to read into R after writing them to a folder chosen via `path`.",
          "i" = "Specify `path` as a string that points to a folder \
          on your computer, or as \".\" for your current working directory."
        ),
        call = rlang::caller_env()
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

  # Simplest case here: just a message (on the bottom). Otherwise, the
  # complexity is so high that the user is asked whether to proceed.
  if (!is.null(msg_wait)) {
    if (need_to_ask) {
      # In memory mode, make sure the user knows there is also the option to
      # write large results to disk.
      if (in_memory_mode) {
        cli::cli_alert_info(paste0(
          "Consider safely writing results to disk by specifying `path` in `",
          caller_fn_name(),
          "()`."
        ))
        cli::cli_alert_info("You would still obtain summary statistics in R.")
        # Empty line before the next alert
        message()
      }

      cli::cli_alert_warning(paste(msg_wait, "Do you wish to proceed?"))

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
      cli::cli_alert(msg_wait)
    }
  }

  # Compute CLOSURE samples by calling into pre-compiled Rust code.
  out <- create_combinations(
    mean = mean_num,
    sd = sd_num,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    rounding_error_mean = rounding_error_mean,
    rounding_error_sd = rounding_error_sd,
    write = parquet_config,
    stop_after = stop_after
  )

  n_samples_all <- if (in_memory_mode) {
    # TODO: Fix this in closure-core; it should be "sample", not "samples"
    names(out$results)[2] <- "sample"
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
        "x" = "Summary statistics internally inconsistent.",
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

  # Assemble the summary statistics that make up much of the output's structure.
  # In memory mode (i.e., without writing to disk), this is done manually here.
  # An S3 class like "closure_generate" is added -- it will be recognized by
  # downstream functions, such as `closure_plot_bar()`. All elements here are
  # created using the low-level `new_tibble()` instead of `tibble()`: once for
  # passing the S3 class, and three times for performance and consistency.
  if (in_memory_mode) {
    out_summary <- list(
      inputs = tibble::new_tibble(
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
        nrow = 1L,
        # The class is, e.g., "closure_generate"
        class = technique |> tolower() |> paste0("_generate")
      ),

      metrics_main = out$metrics_main |>
        as.list() |>
        tibble::new_tibble(nrow = 1L),

      metrics_horns = out$metrics_horns |>
        as.list() |>
        tibble::new_tibble(nrow = 1L),

      frequency = out$frequency |>
        as.list() |>
        tibble::new_tibble(nrow = nrow(out$frequency))
    )

    # In memory mode (i.e., without writing to disk), a message about successful
    # completion is left to display after the rest of the function has finished.
    on.exit({
      # Empty line before the alert
      message()
      cli::cli_alert_success("All {technique} results found")
    })

    # Combine the statistics with the results and return the completed list
    c(
      out_summary,
      list(
        results = tibble::new_tibble(
          x = out$results,
          nrow = n_samples_all
        )
      )
    )
  } else {
    # In writing mode, read the statistics -- and, optionally, results -- that
    # were just written to disk into R. Note that R never held them in memory
    # before this point; they were created on the Rust level. The `include`
    # argument controls which parts of the results are loaded. This is to
    # prevent out-of-memory errors due to large data.
    out_summary <- switch(
      technique,
      "CLOSURE" = closure_read(path_new_dir, include = include),

      # Error if `technique` is allowed but file reading is not supported yet
      cli::cli_abort(
        "Internal error: File reading not supported for {technique}."
      )
    )

    # Overwrite info.txt which now contains instructions for importing the
    # files, etc.; and issue an alert. As both steps give confirmatory signals
    # to the user, this is only done after reading the data successfully.
    write_final_info_txt(path_new_dir, technique)

    # Return the list
    out_summary
  }
}
