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
#'   a new folder with the results. By default, the current working directory.
#'   For `closure_read()`, the path to that new folder.
#'
#' @section Folder name: The new folder's name should be sufficient to recreate
#'   its CLOSURE results. Dashes separate values, underscores replace decimal
#'   periods. For example:
#'
#'   \preformatted{CLOSURE-3_5-1_0-90-1-5-up_or_down-5}
#'
#'   The order is the same as in `closure_generate()`:
#'
#'   \preformatted{
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
#' @details `closure_write()` saves the first three tibbles as CSVs, but the
#'   `"results"` tibble becomes a Parquet file. This is much faster and takes up
#'   far less disk space --- roughly 1% of a CSV file with the same data. Speed
#'   and disk space can be relevant with large result sets.
#'
#'   Use `closure_read()` to recreate the CLOSURE list from the folder. One of
#'   the reasons why it is convenient is that opening a Parquet file requires a
#'   special reader. For a more general tool, see
#'   [`nanoparquet::read_parquet()`].
#'
#' @returns `closure_write()` returns the path to the new folder it created,
#'   `closure_read()` returns a list.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   data <- closure_generate(
#'     mean = "2.7",
#'     sd = "0.6",
#'     n = 45,
#'     scale_min = 1,
#'     scale_max = 5
#'   )
#'
#'   # You should write to a real folder instead;
#'   # or just leave `path` unspecified. I use a
#'   # fake folder just for this example.
#'   path_new_folder <- closure_write(data, path = tempdir())
#'
#'   # In a later session, conveniently read the files
#'   # back into R. This returns the original list,
#'   # identical except for floating-point error.
#'   closure_read(path_new_folder)
#' }

closure_write <- function(data, path = ".") {

  check_closure_generate(data)
  check_value(path, "character")

  slash <- .Platform$file.sep

  # Prepare the name of the new directory to which `data` will be written.
  # Create a string where all the inputs (mean, SD, etc.) are connected through
  # dashes. Then, replace the decimal periods by underscores.
  name_new_dir <- data$inputs |>
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
      )
    )
  }

  dir.create(path_new_dir)

  tibbles_all <- names(data)

  # Write the small tibbles: those other than the "results" samples
  for (tibble in tibbles_all[tibbles_all != "results"]) {
    readr::write_csv(
      x = data[[tibble]],
      file = paste0(path_new_dir, slash, tibble, ".csv")
    )
  }

  # Write the "results" tibble using the efficient Parquet format
  nanoparquet::write_parquet(
    x = data$results$sample |> as_wide_n_tibble(),
    file = paste0(path_new_dir, slash, "results.parquet")
  )

  cli::cli_alert_success("All files written to:\n{path_new_dir}{slash}")

  path_new_dir
}


#' @rdname closure_write
#' @export
closure_read <- function(path) {

  if (!dir.exists(path)) {
    cli::cli_abort(
      message = c(
        "Must choose an existing folder.",
        "x" = "Chosen folder does not exist:",
        "x" = "{path}"
      )
    )
  }

  slash <- .Platform$file.sep

  name_dir <- strsplit(path, slash)[[1]]
  name_dir <- name_dir[length(name_dir)]

  files_all <- dir(path)
  files_expected <- c(
    "inputs.csv",
    "metrics.csv",
    "frequency.csv",
    "results.parquet"
  )

  if (!setequal(files_all, files_expected)) {
    cli::cli_abort(
      message = c(
        "Folder must contain all correct files (and no others).",
        "x" = "Expected files: {files_expected}",
        "x" = "Actual files: {files_all}"
      )
    )
  }

  # Read CSV files into tibbles
  read_small_file <- function(name) {
    path |>
      paste0(slash, name, ".csv") |>
      readr::read_csv(show_col_types = FALSE)
  }

  # Add an S3 class to an object
  add_class <- function(x, new_class) {
    `class<-`(x, value = c(new_class, class(x)))
  }

  out <- list(
    inputs = "inputs" |> read_small_file() |> add_class("closure_generate"),
    metrics = "metrics" |> read_small_file(),
    frequency = "frequency" |> read_small_file(),

    results = path |>
      paste0(slash, "results.parquet") |>
      nanoparquet::read_parquet() |>
      as_results_tibble()
  )

  # Parse mean and SD from the folder name
  mean_sd_str <- strsplit(name_dir, "-")[[1]][2:3] |>
    gsub("_", "\\.", x = _)

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
      out$metrics$samples_initial <- as.integer(out$metrics$samples_initial)
      out$metrics$samples_all <- as.integer(out$metrics$samples_all)
      out$metrics$values_all <- as.integer(out$metrics$values_all)
    },
    error = function(e) {
      cli::cli_abort(
        " \"metrics\" must have \"samples_initial\", \
        \"samples_all\", and \"values_all\" columns."
      )
    }
  )

  tryCatch(
    expr = {
      out$frequency$value <- as.integer(out$frequency$value)
      out$frequency$f_absolute <- as.integer(out$frequency$f_absolute)
    },
    error = function(e) {
      cli::cli_abort(
        "\"frequency\" must have \"value\" and \"f_absolute\" columns."
      )
    }
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

