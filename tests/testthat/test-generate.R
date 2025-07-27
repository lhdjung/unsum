
# Use R wrapper around Rust implementation
data_r <- closure_generate(
  mean = "3.5",
  sd = "0.5",
  n = 52,
  scale_min = 1,
  scale_max = 5
)

test_that("The results pass unsum's internal check for `closure_generate()` output", {
  data_r |> check_closure_generate() |> expect_no_error()
})

# Adjust results of R wrapper to format of data saved on disk
data_r <- data_r$results$sample |> as_wide_n_tibble()

# Check results for identity after sorting columns. Different CLOSURE
# implementations may yield results in different order (even though the samples
# are pairwise identical) because of the details of how parallel processing
# works in each implementation. In other words, the columns must be identical
# when ordered, but only then. Spurious differences arising from such ordering
# effects are ignored below.

test_that("All implementations return identical results (after sorting columns)", {
  .data_rust   |> identical_sorted_cols(data_r) |> expect_true()
  .data_python |> identical_sorted_cols(data_r) |> expect_true()
})

# # If any differences occur, investigate them in detail. This is only meant to
# # be used interactively -- incomment if needed, then outcomment again.
# .data_rust   <- sort_cols(.data_rust)
# .data_python <- sort_cols(.data_python)
# data_r       <- sort_cols(data_r)
# waldo::compare(.data_rust,   .data_python, x_arg = "rust",   y_arg = "python")
# waldo::compare(.data_rust,   data_r,       x_arg = "rust",   y_arg = "r")
# waldo::compare(.data_python, data_r,       x_arg = "python", y_arg = "r")


f_absolute_centered <- closure_generate(
  mean = "4.0",
  sd = "1.0",
  n = 50,
  scale_min = 1,
  scale_max = 7
)$frequency$f_absolute

f_absolute_skewed_left <- closure_generate(
  mean = "2.5",
  sd = "1.7",
  n = 50,
  scale_min = 1,
  scale_max = 7
)$frequency$f_absolute

f_absolute_skewed_right <- closure_generate(
  mean = "5.3",
  sd = "1.7",
  n = 50,
  scale_min = 1,
  scale_max = 7
)$frequency$f_absolute


test_that("absolute frequencies are correct", {
  f_absolute_centered |> expect_equal(c(
    6575L, 17570L, 65388L, 209734L, 65388L, 17570L, 6575L, 333L, 920L, 3493L,
    13108L, 3493L, 920L, 333L, 382L, 991L, 3587L, 10430L, 3587L, 991L, 382L
  ))

  f_absolute_skewed_left  |> expect_equal(c(
    571399L, 341334L, 194430L, 147830L, 107079L, 69040L, 44938L, 27281L, 18458L,
    10370L, 7729L, 5225L, 3212L, 2075L, 15109L, 7965L, 4557L, 3615L, 2784L, 1830L,
    1240L
  ))

  f_absolute_skewed_right |> expect_equal(c(
    80279L, 123068L, 191619L, 269477L, 357529L, 626871L, 708557L, 1970L, 3059L,
    4979L, 7495L, 10162L, 17953L, 16332L, 2320L, 3537L, 5252L, 6862L, 9021L,
    15508L, 18850L
  ))
})


