# Note: some helper functions called here can be found in the R/utils.R file.
# The most notable exception `create_combinations()`, is in
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
# rounding = "up_or_down"
# threshold = 5
# path <- "."
# include <- "stats_and_horns"
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
  check_value(n, "double")
  check_value(scale_min, "double")
  check_value(scale_max, "double")
  check_value(path, "character", allow_null = TRUE)
  check_value(rounding, "character")
  check_value(threshold, "double")
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
        mean = mean,
        sd = sd,
        n = n,
        scale_min = scale_min,
        scale_max = scale_max,
        rounding = rounding,
        threshold = threshold
      ),
      path = path,
      technique = technique
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
    0  # default case
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
    write = parquet_config
  )

  n_samples_all <- if (in_memory_mode) {
    # TODO: Fix this in closure-core; it should be "sample", not "samples"
    names(out$results)[2] <- "sample"
    length(out$results$sample)
  } else {
    out$total_combinations
  }

  # By default, raise a warning if no results were found. Invalid when writing
  # to disk (hence the last check) because, in this case, the samples won't be
  # stored in `results`.
  if (n_samples_all == 0) {
    cli::cli_warn(c(
      "No results found with these inputs.",
      "x" = "Data internally inconsistent.",
      "x" = "These statistics can't describe the same distribution."
    ))
  }

  # Insert the samples into a data frame, along with summary statistics. The S3
  # class "closure_generate" will be recognized by downstream functions, such as
  # `closure_plot_bar()`. All elements here are created using the low-level
  # `new_tibble()` instead of `tibble()`: once for passing the S3 class, and
  # three times for performance.
  out_summary <- if (in_memory_mode) {
    list(
      inputs = tibble::new_tibble(
        x = list(
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
        class = paste0(tolower(technique), "_generate")
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
  } else {
    closure_read(path_new_dir, include = include)
  }

  # In memory mode (i.e., without writing to disk), a message about successful
  # completion is left to display after the rest of the function has finished.
  # In writing mode, the results were written already, so all that is left is to
  # overwrite info.txt (now informing about how to import the files, etc.), and
  # issue an alert. Finally, return the results, which are partial by default.
  if (in_memory_mode) {
    on.exit({
      # Empty line before the alert
      message()
      cli::cli_alert_success("All {technique} results found")
    })
  } else {
    overwrite_info_txt(path_new_dir, technique)
    return(out_summary)
  }

  # Insert the samples into a data frame, along with summary statistics. The S3
  # class "closure_generate" will be recognized by downstream functions, such as
  # `closure_plot_bar()`. All elements here are created using the low-level
  # `new_tibble()` instead of `tibble()`: once for passing the S3 class, and
  # four times for performance.
  c(
    out_summary,
    list(
      results = tibble::new_tibble(
        x = out$results,
        nrow = n_samples_all
      )
    )
  )
}


#' Generate CLOSURE samples
#'
#' @description Call `closure_generate()` to run the CLOSURE algorithm on a
#'   given set of summary statistics.
#'
#'   This can take seconds, minutes, or longer, depending on the input. Wide
#'   variance and large `n` often lead to many samples, i.e., long runtimes.
#'   These effects interact dynamically. For example, with large `n`, even very
#'   small increases in `sd` can greatly increase runtime and number of values
#'   found. Consider specifying `path` in these cases; see "Writing to disk"
#'   below.
#'
#'   If the inputs are inconsistent, there is no solution. The function will
#'   then return empty results and throw a warning.
#'
#' @param mean String (length 1). Reported mean.
#' @param sd String (length 1). Reported sample standard deviation.
#' @param n Numeric (length 1). Reported sample size.
#' @param scale_min,scale_max Numeric (length 1 each). Minimal and maximal
#'   possible values. For example, with a 1-7 Likert scale, use `scale_min = 1`
#'   and `scale_max = 7`. Prefer the empirical min and max if available: they
#'   constrain the possible values further.
#' @param path String (length 1). Optionally, choose the directory where a new
#'   folder with CLOSURE results should be created. Use `path = "."` for your
#'   current working directory. See "Writing to disk" below.
#' @param include String (length 1). If results are written to disk, which parts
#'   of them should be included in the R output?
#'   - With `"stats_and_horns"`, the default, all parts except for the samples
#'   are included.
#'   - `"stats_only"` excludes the `"results"` tibble, i.e., samples and horns.
#'   - `"all"` reads the full results, including the samples and horns values.
#' @param rounding String (length 1). Rounding method assumed to have created
#'   `mean` and `sd`. See [*Rounding
#'   options*](https://lhdjung.github.io/roundwork/articles/rounding-options.html),
#'   but also the *Rounding limitations* section below. Default is
#'   `"up_or_down"` which, e.g., unrounds `0.12` to `0.115` as a lower bound and
#'   `0.125` as an upper bound.
#' @param threshold Numeric (length 1). Number from which to round up or down,
#'   if `rounding` is any of `"up_or_down"`, `"up"`, and `"down"`. Default is
#'   `5`.
#' @param ask_to_proceed Logical (length 1). If the runtime is predicted to be
#'   very long in an interactive setting, should the function prompt you to
#'   proceed or abort? Default is `TRUE`.
#'
#' @section Writing to disk: Specify `path` if the expected runtime is very
#'   long. (In case you have trouble choosing a path, use `path = "."` for your
#'   current working directory.) This makes sure the results are preserved by
#'   incrementally writing them to disk. Otherwise, you might encounter an
#'   out-of-memory error because `closure_generate()` accumulates more data than
#'   your computer can hold in memory.
#'
#'   If `path` is specified, the output in R will not include the `"sample"`
#'   column in the `results` tibble by default; but it will feature a
#'   `directory` tibble with the file path. If you choose to load the samples
#'   using `include = "all"`, you incur the risk of a memory error. Even so, the
#'   results are safe because they are written to disk first; and you can later
#'   try to load the full detailed results from disk using [`closure_read()`]
#'   and its `include` argument. This would separate saving the data to disk
#'   from an uncertain attempt to load them into R.
#'
#' @section More about memory: Some output columns that contain counts, such as
#'   `f_absolute`, are doubles instead of integers. This is because doubles are
#'   able to contain much larger numbers. When counting CLOSURE results, it is
#'   not unrealistic to reach the limit of 32-bit integers in R, which is
#'   roughly two billion.
#'
#' @section Rounding limitations: The `rounding` and `threshold` arguments are
#'   not fully implemented. For example, CLOSURE currently treats all rounding
#'   bounds as inclusive, even if the `rounding` value would imply otherwise.
#'   Many specifications of the two arguments will not make any difference, and
#'   those that do will most likely lead to empty results.
#'
#' @return `closure_generate()` returns a named list of tibbles (data frames):
#'   - **`inputs`**: Arguments to this function.
#'   - **`metrics_main`**:
#'     - `samples_initial`: integer. The basis for computing CLOSURE results,
#'   based on scale range only. See [`closure_count_initial()`].
#'     - `samples_all`: double. Number of all samples. Equal to the number
#'   of rows in `results`.
#'     - `values_all`: double. Number of all individual values found. Equal to
#'   `n * samples_all`.
#'   - **`metrics_horns`**:
#'     - `mean`: double. Average horns value of all samples. The horns index is
#'   a measure of dispersion for bounded scales; see [`horns()`].
#'     - `uniform`: double. The value that `mean` would have if all samples were
#'   uniformly distributed; see [`horns_uniform()`].
#'     - `sd`, `cv`, `mad`, `min`, `median`, `max`, `range`: double. Standard
#'   deviation, coefficient of variation, median absolute deviation, minimum,
#'   median, maximum, and range of the horns index values across all samples.
#'   Note that `mad` is not scaled using a constant, as [`stats::mad()`] is by
#'   default.
#'   - **`frequency`**:
#'     - `samples`: string. Frequencies apply to one of three subsets of
#'   samples: `"all"` for all samples, `"horns_min"` for those samples with the
#'   lowest horns index among all samples, and `"horns_max"` for those samples
#'   with the highest horns index.
#'     - `value`: integer. Scale values derived from `scale_min` and
#'   `scale_max`.
#'     - `f_average`: double. Count of scale values in the mean `results`
#'   sample.
#'     - `f_absolute`: double. Count of individual scale values found in the
#'   `results` samples.
#'     - `f_relative`: double. Values' share of total values found.
#'   - **`results`**:
#'     - `id`: integer. Runs from `1` to `samples_all`.
#'     - `sample` (not present by default if `path` was specified): list of
#'   integer vectors. Each of these vectors has length `n`. It is a sample (or
#'   distribution) of individual scale values found by CLOSURE.
#'     - `horns`: double. Horns index of each sample.
#'   - **`directory`** (only present if `path` was specified):
#'     - `path`: string. Location of the folder in which the results were saved.
#'
#' @include utils.R count.R horns.R performance.R read-write.R
#'   extendr-wrappers.R
#'
#' @export
#'
#' @examples
#' # High spread often leads to many samples --
#' # here, 2492.
#' data_high <- closure_generate(
#'   mean = "3.5",
#'   sd = "1.7",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' data_high
#'
#' # Get a clear picture of the distribution
#' # by following up with `closure_plot_bar()`:
#' closure_plot_bar(data_high)
#'
#' # Low spread, only 4 samples, and not all
#' # scale values are possible.
#' data_low <- closure_generate(
#'   mean = "2.9",
#'   sd = "0.5",
#'   n = 25,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' data_low
#'
#' # This can also be shown by `closure_plot_bar()`:
#' closure_plot_bar(data_low)

closure_generate <- function() {
  generate_from_mean_sd_n(
    mean = mean,
    sd = sd,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    technique = "CLOSURE",
    path = path,
    include = include,
    rounding = rounding,
    threshold = threshold,
    ask_to_proceed = ask_to_proceed,
    rounding_error_mean = NULL,
    rounding_error_sd = NULL
  )
}

formals(closure_generate) <- generate_from_mean_sd_n |>
  formals() |>
  formals_remove(
    "technique",
    "rounding_error_mean",
    "rounding_error_sd"
  )
