#' Write CLOSURE results to disk
#'
#' @description You can use `closure_write()` to save the results of
#'   [`closure_combine()`] on your computer. A message will show the exact
#'   location.
#'
#'   The data are saved in a new folder as four separate files, one for each
#'   tibble in `closure_combine()`'s output. The folder is named after the
#'   parameters of `closure_combine()`.
#'
#'
#' @param data List returned by `closure_combine()`.
#' @param path String (length 1). File path where a new folder with the results
#'   will be created. By default, the current working directory.
#'
#' @section Folder name: The new folder's name should be sufficient to recreate
#'   its CLOSURE results. Dashes separate values, underscores replace decimal
#'   periods. For example:
#'
#'   \preformatted{CLOSURE-3_5-1_0-90-1-5-up_or_down-5}
#'
#'   The order is the same as in `closure_combine()`:
#'
#'   \preformatted{
#'   closure_combine(
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
#' @details The first three tibbles are saved as CSVs, but the `"results"`
#'   tibble becomes a Parquet file. This is much faster and takes up far less
#'   disk space --- roughly 1% of a CSV file with the same data. Speed and disk
#'   space can be relevant with large result sets.
#'
#'   Opening a Parquet file requires a special reader. Inside of R, you could
#'   use [`nanoparquet::read_parquet()`].
#'
#' @returns No return value, called for side effects.
#' @export
closure_write <- function(data, path = ".") {

  check_closure_combine(data)
  check_value(path, "character")

  slash <- if (Sys.info()[["sysname"]] == "Windows") "\\" else "/"

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
    x = format_n_cols(data$results$combination),
    file = paste0(path_new_dir, slash, "results.parquet")
  )

  cli::cli_alert_success("All files written to:\n{path_new_dir}{slash}")
}
