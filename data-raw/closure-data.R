## code to prepare datasets containing results of other CLOSURE implementations
## goes here

# Note: These data should be equivalent to these two data frames:
.data_r_1 <- closure_generate(
  mean = "3.5",
  sd = "0.5",
  n = 52,
  scale_min = 1,
  scale_max = 5
)$results$sample |>
  as_wide_n_tibble()

.data_r_2 <- closure_generate(
  mean = "3.7",
  sd = "1.2",
  n = 120,
  scale_min = 1,
  scale_max = 5,
)$results$sample |>
  as_wide_n_tibble()


# Read local CLOSURE files written by the Rust and Python implementations. The
# `number` argument is the count of the current set of data frames, e.g., `1`
# for `.data_r_1` and the same in Rust and Python.
read_local_results <- function(
  number,
  version = c("rust", "python"),
  folder_rust = "/home/lukas/Documents/rust_projects/closure-core",
  folder_python = "/home/lukas/Documents/python_projects/CLOSURE-Python"
) {
  version <- rlang::arg_match(version)

  # Part of the path (to a local file with CLOSURE results) before "1.csv" or
  # "2.csv" -- which will be added below
  path_trunk <- if (version == "rust") {
    folder_rust
  } else if (version == "python") {
    folder_python
  } else {
    cli::cli_abort("Internal error: invalid `version` argument")
  }

  slash <- .Platform$file.sep

  # Complete file path
  path_full <- paste0(path_trunk, slash, "parallel_results_", number, ".csv")

  # Read all columns as integer
  readr::read_csv(path_full, col_types = readr::cols(.default = "i"))
}


# Compare the results produced in R, Rust, and Python. If any two datasets are
# different (except in terms of the order in their columns; this doesn't
# matter), show how exactly they differ, then throw an error.
check_results_equal <- function(r, rust, python) {
  name_r <- deparse(substitute(r))
  name_rust <- deparse(substitute(rust))
  name_python <- deparse(substitute(python))

  if (identical_sorted_cols(r, rust)) {
    cli::cli_alert_success(
      "R and Rust results are equal: {name_r} and {name_rust}"
    )
  } else {
    message(waldo::compare(r, rust, x_arg = "R", y_arg = "Rust"))
    cli::cli_abort("`{name_r}` and `{name_rust}` are different results!")
  }

  if (identical_sorted_cols(rust, python)) {
    cli::cli_alert_success(
      "Rust and Python results are equal: {name_rust} and {name_python}"
    )
  } else {
    message(waldo::compare(rust, python, x_arg = "Rust", y_arg = "Python"))
    cli::cli_abort("`{name_rust}` and `{name_python}` are different results!")
  }
}


# Load CLOSURE output data of Rust and Python implementations
.data_rust_1 <- read_local_results(1, "rust")
.data_rust_2 <- read_local_results(2, "rust")

.data_python_1 <- read_local_results(1, "python")
.data_python_2 <- read_local_results(2, "python")


# Compare to check for equality
check_results_equal(.data_r_1, .data_rust_1, .data_python_1)
check_results_equal(.data_r_2, .data_rust_2, .data_python_2)


# Save these datasets to disk. They will be internally available to the package,
# so they can be used by unit tests. However, they are not exported -- this is
# why their names start with dots. (For safety, this code will fail as is
# because of the existing R/sysdata.rda; only add `overwrite = TRUE` if you are
# really sure!)
usethis::use_data(
  .data_rust_1,
  .data_rust_2,
  internal = TRUE
)
