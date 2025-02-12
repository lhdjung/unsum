
#' @include utils.R

# Placeholder implementation for now -- should later use Parquet or so; or maybe
# don't expose file reading to the user at all? Just let the Rust code do it
# under the hood?
closure_read <- function(version = c("new", "old", "python")) {

  version <- rlang::arg_match(version)

  infix <- if (version == "old") {
    "_old"
  } else {
    ""
  }

  path <- if (version == "python") {
    "/home/lukas/Documents/python_projects/CLOSURE-Python/parallel_results.csv"
  } else {
    paste0(
      "/home/lukas/Documents/rust_projects/closure-core/parallel_results",
      infix,
      ".csv"
    )
  }

  # Read all columns as integer:
  path %>%
    readr::read_csv(col_types = readr::cols(.default = "i"))
    # dplyr::slice(1:5000) %>%
}


# # Load CLOSURE output data
# data_rust   <- closure_read("new")
# data_python <- closure_read("python")
# data_r <- closure_combine(
#   mean = "3.5",
#   sd = "0.5",
#   n = 52,
#   scale_min = 1,
#   scale_max = 5
# )
# data_r <- data_r$results$combination %>%
#   tibble::as_tibble(.name_repair = "minimal") %>%
#   t() %>%
#   tibble::as_tibble(.name_repair = function(x) paste0("n", seq_along(x)))
#
# identical_except_attributes(data_rust, data_python)
# waldo::compare(data_rust, data_python, x_arg = "rust", y_arg = "python")
# waldo::compare(data_rust, data_r, x_arg = "rust", y_arg = "r")


