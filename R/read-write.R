#' Write CLOSURE results to disk (and read them back in)
#'
#' @description You can use `closure_write()` to save the results of
#'   [`closure_generate()`] on your computer. A message will show the exact
#'   location.
#'
#'   The data are saved in a new folder as four separate files, one for each
#'   tibble in `closure_generate()`'s output. The folder is named after the
#'   parameters of `closure_generate()`.
#'
#'   `closure_read()` is the opposite: it reads those files back into R,
#'   recreating the original CLOSURE list. This is useful for later analyses if
#'   you don't want to re-run a lengthy `closure_generate()` call.
#'
#' @param data List returned by `closure_generate()`.
#' @param path String (length 1). File path where `closure_write()` will create
#'   a new folder with the results. Set it to `"."` to choose the current
#'   working directory. For `closure_read()`, the path to an existing folder
#'   with results.
#' @param include String (length 1). Which parts of the detailed results should
#'   be read in?
#'   - With `"stats_only"`, the default, no results are read.
#'   - `"stats_and_horns"` reads the horns index values, but not the samples.
#'   - `"capped_error"` checks whether the number of samples is higher than a
#'   given threshold (see `samples_cap`). If so, it throws an error; but if not,
#'   it reads both the samples and the horns values.
#'   - `"all"` reads both the samples and the horns values.
#' @param samples_cap Numeric (length 1). When using `include = "capped_error"`,
#'   enter a whole number here to specify a cap. Default is `NULL`.
#'
#' @section Folder name: The new folder's name will contain all the inputs that
#'   determine the CLOSURE results. Dashes separate values and underscores
#'   replace decimal periods. For example:
#'
#'   \preformatted{
#'
#'   CLOSURE-3_5-1_0-90-1-5-up_or_down-5
#'   }
#'
#'   The order is the same as in `closure_generate()`:
#'
#'   \preformatted{
#'
#'   closure_generate(
#'     mean = "3.5",
#'     sd = "1.0",
#'     n = 90,
#'     scale_min = 1,
#'     scale_max = 5,
#'     rounding = "up_or_down",  # default
#'     threshold = 5             # default
#'   )
#'  }
#'
#' @details `closure_write()` saves all tibbles as Parquet files. This is much
#'   faster and takes up far less disk space --- roughly 1% of a CSV file with
#'   the same data. Speed and disk space can be relevant with large result sets.
#'
#'   Use `closure_read()` to recreate the CLOSURE list from the folder. One of
#'   the reasons why it is convenient is that opening a Parquet file requires a
#'   special reader. For a more general tool, see
#'   [`nanoparquet::read_parquet()`].
#'
#' @returns
#'   - `closure_write()` returns the path to the new folder it created.
#'   - `closure_read()` returns a list of the same kind as
#' [`closure_generate()`].
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' data <- closure_generate(
#'   mean = "2.7",
#'   sd = "0.6",
#'   n = 45,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Writing to a temporary folder just for this example.
#' # You should write to a real folder instead.
#' # A simple way is path = "." for your current directory.
#' path_new_folder <- closure_write(data, path = tempdir())
#'
#' # In a later session, conveniently read the files
#' # back into R. This returns the original list,
#' # identical except for floating-point error.
#' # (Of course, the `path_new_folder` variable will
#' # no longer be available -- instead, paste the path
#' # to your folder here.)
#' closure_read(path_new_folder)

closure_write <- function(data, path) {
  check_closure_generate(data)
  check_value(path, "character")

  # Translate "." to the user's working directory. If the path was manually
  # supplied, remove leading or trailing whitespace, including linebreaks.
  path <- if (path == ".") {
    getwd()
  } else {
    trimws(path)
  }

  # "closure_generate" --> "CLOSURE" etc.
  technique <- data$inputs |>
    class() |>
    (function(x) x[grepl("_generate$", x)])() |>
    sub("_generate$", "", x = _) |>
    toupper()

  has_reading_class <- data$inputs |>
    class() |>
    grepl("^closure_read_include_", x = _) |>
    any()

  # Refuse to rewrite data that were already saved to disk
  if (
    has_reading_class &&
      any(names(data) == "directory") &&
      any(names(data$directory) == "path")
  ) {
    cli::cli_abort(
      message = c(
        "Results already saved on disk.",
        "x" = "Folder with {technique} results present at:",
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

  # Prefix with the name of the technique to make the origin very clear
  name_new_dir <- paste0(technique, "-", name_new_dir)

  # Full path of the new directory, not just the name
  path_new_dir <- paste0(
    path,
    slash,
    name_new_dir,
    slash
  )

  create_results_folder(path_new_dir)

  tibbles_all <- names(data)

  # Write the small tibbles: those other than the "results"
  for (tibble in tibbles_all[tibbles_all != "results"]) {
    nanoparquet::write_parquet(
      data[[tibble]],
      file = paste0(path_new_dir, tibble, ".parquet")
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

  # Create info.txt and issue an alert. Leave an empty line before.
  message()
  write_final_info_txt(path_new_dir, technique)

  path_new_dir
}


# path <- "/home/lukas/Documents/r_projects/packages/unsum/CLOSURE-5_00-2_78-30-1-8-up_or_down-5"
# include <- "stats_only"
# samples_cap <- NULL

#' @rdname closure_write
#' @export
closure_read <- function(
  path,
  include = c("stats_only", "stats_and_horns", "capped_error", "all"),
  samples_cap = NULL
) {
  include <- rlang::arg_match(include)

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
    cli::cli_abort(
      message = c(
        "If `include` is \"{include}\", `samples_cap` must be specified.",
        "i" = "Use `samples_cap` to state a threshold -- if there are \
        more than this many samples, there will be an error."
      )
    )
  }

  if (include != "capped_error" && !is.null(samples_cap)) {
    cli::cli_abort(
      message = c(
        "If `samples_cap` is specified, `include` must be \"capped_error\".",
        "x" = "`include` is: \"{include}\"",
        "x" = "`samples_cap` is: `{samples_cap}`"
      )
    )
  }

  if (!dir.exists(path)) {
    cli::cli_abort(
      message = c(
        "Must choose an existing folder.",
        "x" = "Chosen folder does not exist:",
        "x" = path
      )
    )
  }

  slash <- .Platform$file.sep

  name_dir <- strsplit(path, slash)[[1]]
  name_dir <- name_dir[length(name_dir)]

  files_all <- dir(path)
  files_expected <- c(
    "info.txt",
    "inputs.parquet",
    "metrics_main.parquet",
    "metrics_horns.parquet",
    "frequency.parquet",
    "horns.parquet",
    "samples.parquet"
  )

  # Error if the folder contains other files than those needed, or if it does
  # not contain all of those needed. A bespoke message is shown in each case.
  if (!setequal(files_all, files_expected)) {
    files_expected <- sort(files_expected)
    files_all <- sort(files_all)

    offenders_missing <- setdiff(files_expected, files_all)
    offenders_not_needed <- setdiff(files_all, files_expected)

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
      message = c(
        "Folder must contain all correct files (and no others).",
        "!" = "Expected files: {files_expected}",
        msg_missing,
        msg_not_needed
      )
    )
  }

  # Function to add an S3 class to an object
  add_class <- function(x, new_class) {
    `class<-`(x, value = c(new_class, class(x)))
  }

  # Function to read the small Parquet files into tibbles. Add the "tbl_df"
  # class which is not included by `read_parquet()` but is needed for regular
  # tibbles.
  read_file <- function(name) {
    path |>
      paste0(slash, name, ".parquet") |>
      nanoparquet::read_parquet() |>
      add_class("tbl_df")
  }

  # Carry out these two steps. At this point in time, `out` corresponds to
  # `include == "stats_only"` because all of the additions further below
  # correspond to other variants of `include`.
  out <- list(
    inputs = "inputs" |>
      read_file() |>
      add_class(c(
        paste0("closure_read_include_", include),
        "closure_generate"
      )),

    metrics_main = "metrics_main" |> read_file(),
    metrics_horns = "metrics_horns" |> read_file(),
    frequency = "frequency" |> read_file()
  )

  # For consistency with `closure_generate()`
  out$metrics_main$samples_all <- as.double(out$metrics_main$samples_all)
  out$metrics_main$values_all <- as.double(out$metrics_main$values_all)

  # Parse mean and SD from the folder name
  mean_sd_str <- name_dir |>
    strsplit("-") |>
    (function(x) x[[1]][2:3])() |>
    gsub("_", "\\.", x = _)

  # Check that files read from disk are correct
  if (
    !near(as.numeric(mean_sd_str[1]), as.numeric(out$inputs$mean)) ||
    !near(as.numeric(mean_sd_str[2]), as.numeric(out$inputs$sd))
  ) {
    cli::cli_abort(
      "Mean and SD in inputs.csv must match those \
      in the folder's name."
    )
  }

  tryCatch(
    expr = {
      out$inputs$mean <- mean_sd_str[1]
      out$inputs$sd <- mean_sd_str[2]
    },
    error = function(e) {
      cli::cli_abort("\"inputs\" must have \"mean\" and \"sd\" columns.")
    }
  )

  tryCatch(
    expr = {
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
        id = seq_len(n_samples_all),
        horns = nanoparquet::read_parquet(path_horns)[[1]]
      ),
      nrow = n_samples_all
    )
  } else if (include == "capped_error" && n_samples_all > samples_cap) {
    cli::cli_abort(
      message = c(
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
        id = seq_len(n_samples_all),

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
                message = c(
                  "Reading samples.parquet from disk failed.",
                  "x" = "Original error:",
                  "x" = e,
                  "i" = "If memory is lacking, try \
                  `include = \"stats_only\"` or \
                  `include = \"stats_and_horns\"`."
                )
              )
            }
          ),

        # Horns index values (3 / 3)
        horns = nanoparquet::read_parquet(path_horns)[[1]]
      ),
      nrow = n_samples_all
    )
  } else if (include != "stats_only") {
    cli::cli_abort(
      message = "Internal error: invalid `include` variant \"{include}\""
    )
  }

  # Add a record of the folder's path
  out$directory <- tibble::new_tibble(
    x = list(path = path),
    nrow = 1L
  )

  # Final check -- is the reconstructed list correct?
  tryCatch(
    expr = check_closure_generate(out),
    error = function(e) {
      cli::cli_abort(
        message = c(
          "Something went wrong when reading from disk.",
          "x" = "Original error:",
          "x" = "{e}"
        )
      )
    }
  )

  out
}
