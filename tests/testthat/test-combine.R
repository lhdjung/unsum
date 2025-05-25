
# Use R wrapper around Rust implementation
data_r <- closure_combine(
  mean = "3.5",
  sd = "0.5",
  n = 52,
  scale_min = 1,
  scale_max = 5
)

# Adjust results of R wrapper to format of data saved on disk
data_r <- format_n_cols(data_r)

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


f_absolute_centered <- closure_combine(
  mean = "4.0",
  sd = "1.0",
  n = 50,
  scale_min = 1,
  scale_max = 7
)$frequency$f_absolute

f_absolute_skewed_left <- closure_combine(
  mean = "2.5",
  sd = "1.7",
  n = 50,
  scale_min = 1,
  scale_max = 7
)$frequency$f_absolute

f_absolute_skewed_right <- closure_combine(
  mean = "5.3",
  sd = "1.7",
  n = 50,
  scale_min = 1,
  scale_max = 7
)$frequency$f_absolute


test_that("absolute frequencies are correct", {
  f_absolute_centered     |> expect_equal(c(6575L, 17570L, 65388L, 209734L, 65388L, 17570L, 6575L))
  f_absolute_skewed_left  |> expect_equal(c(571399L, 341334L, 194430L, 147830L, 107079L, 69040L, 44938L))
  f_absolute_skewed_right |> expect_equal(c(80279L, 123068L, 191619L, 269477L, 357529L, 626871L, 708557L))
})


