## code to prepare datasets containing results of other CLOSURE implementations
## goes here

# # Note: These data should be equivalent to:
# closure_combine(
#   mean = "3.5",
#   sd = "0.5",
#   n = 52,
#   scale_min = 1,
#   scale_max = 5
# )

closure_read <- function(version = c("rust", "python")) {

  version <- rlang::arg_match(version)

  # Load local file. Modify paths if needed.
  path <- if (version == "rust") {
    "/home/lukas/Documents/rust_projects/closure-core/parallel_results.csv"
  } else if (version == "python") {
    "/home/lukas/Documents/python_projects/CLOSURE-Python/parallel_results.csv"
  } else {
    stop("Invalid `version` argument")
  }

  # Read all columns as integer:
  readr::read_csv(path, col_types = readr::cols(.default = "i"))
}


# Load CLOSURE output data of Rust and Python implementations
.data_rust   <- closure_read("rust")
.data_python <- closure_read("python")

# Save these datasets to disk. They will be internally available to the package,
# so they can be used by unit tests. However, they are not exported -- this is
# why their names start with dots.
usethis::use_data(
  .data_rust,
  .data_python,
  internal = TRUE
)

