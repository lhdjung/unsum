
# Use R wrapper around Rust implementation
data_r <- closure_combine(
  mean = "3.5",
  sd = "0.5",
  n = 52,
  scale_min = 1,
  scale_max = 5
)

# Adjust results of R wrapper to format of data saved on disk
data_r <- data_r$results$combination %>%
  tibble::as_tibble(.name_repair = "minimal") %>%
  t() %>%
  tibble::as_tibble(.name_repair = function(x) paste0("n", seq_along(x)))


# Check results for identity after sorting columns. Different CLOSURE
# implementations may yield results in different order (even though the samples
# are pairwise identical) because of the details of how parallel processing
# works in each implementation. In other words, the columns must be identical
# when ordered, but only then. Spurious differences arising from such ordering
# effects are ignored below.

test_that("All implementations return identical results (after sorting columns)", {
  .data_rust   %>% identical_sorted_cols(data_r) %>% expect_true()
  .data_python %>% identical_sorted_cols(data_r) %>% expect_true()
})

# # If any differences occur, investigate them in detail:
# .data_rust   <- sort_cols(.data_rust)
# .data_python <- sort_cols(.data_python)
# data_r       <- sort_cols(data_r)
# waldo::compare(.data_rust,   .data_python, x_arg = "rust",   y_arg = "python")
# waldo::compare(.data_rust,   data_r,       x_arg = "rust",   y_arg = "r")
# waldo::compare(.data_python, data_r,       x_arg = "python", y_arg = "r")
