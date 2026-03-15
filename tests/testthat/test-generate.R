# CLOSURE -----------------------------------------------------------------

# Use R wrapper around Rust implementation
data_r_1 <- closure_generate(
  mean = "3.5",
  sd = "0.5",
  n = 52,
  scale_min = 1,
  scale_max = 5
)


data_r_2 <- closure_generate(
  mean = "3.7",
  sd = "1.2",
  n = 120,
  scale_min = 1,
  scale_max = 5,
)


test_that("The results pass unsum's internal check for `closure_generate()` output", {
  data_r_1 |> check_generator_output("CLOSURE") |> expect_no_error()
  data_r_2 |> check_generator_output("CLOSURE") |> expect_no_error()
})


maybe_wrong_1 <- data_r_1 |> count_wrong_stats()
maybe_wrong_2 <- data_r_2 |> count_wrong_stats()

test_that("All samples yield the original mean and SD", {
  maybe_wrong_1 |> any_wrong_stats() |> expect_false()
  maybe_wrong_2 |> any_wrong_stats() |> expect_false()
})


# Adjust results of R wrapper to format of data saved on disk
wide_n_1 <- data_r_1$results$sample |> as_wide_n_tibble()
wide_n_2 <- data_r_2$results$sample |> as_wide_n_tibble()


# Check results for identity after sorting columns. Different CLOSURE
# implementations may yield results in different order (even though the samples
# are pairwise identical) because of the details of how parallel processing
# works in each implementation. In other words, the columns must be identical
# when ordered, but only then. Spurious differences arising from such ordering
# effects are ignored below.

ok <- test_that("All implementations return identical results (after sorting the columns)", {
  .data_rust_1 |> identical_sorted_cols(wide_n_1) |> expect_true()
  .data_rust_2 |> identical_sorted_cols(wide_n_2) |> expect_true()
})


# If any differences occur, display them in detail
if (!ok) {
  .data_rust_1 <- sort_cols(.data_rust_1)
  .data_python_1 <- sort_cols(.data_python_1)
  wide_n_1 <- sort_cols(wide_n_1)

  all.equal(.data_rust_1, .data_python_1, x_arg = "rust", y_arg = "python")
  all.equal(.data_rust_1, wide_n_1, x_arg = "rust", y_arg = "r")
  all.equal(.data_python_1, wide_n_1, x_arg = "python", y_arg = "r")
}


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
  f_absolute_centered |>
    expect_equal(c(
      6575L,
      17570L,
      65388L,
      209734L,
      65388L,
      17570L,
      6575L,
      333L,
      920L,
      3493L,
      13108L,
      3493L,
      920L,
      333L,
      382L,
      991L,
      3587L,
      10430L,
      3587L,
      991L,
      382L
    ))

  f_absolute_skewed_left |>
    expect_equal(c(
      571399L,
      341334L,
      194430L,
      147830L,
      107079L,
      69040L,
      44938L,
      27281L,
      18458L,
      10370L,
      7729L,
      5225L,
      3212L,
      2075L,
      15109L,
      7965L,
      4557L,
      3615L,
      2784L,
      1830L,
      1240L
    ))

  f_absolute_skewed_right |>
    expect_equal(c(
      80279L,
      123068L,
      191619L,
      269477L,
      357529L,
      626871L,
      708557L,
      1970L,
      3059L,
      4979L,
      7495L,
      10162L,
      17953L,
      16332L,
      2320L,
      3537L,
      5252L,
      6862L,
      9021L,
      15508L,
      18850L
    ))
})


# SPRITE ------------------------------------------------------------------

plot_bar_both <- TRUE

inputs_constant <- list(
  mean = "3.5",
  sd = "1.7",
  n = 75,
  scale_min = 1,
  scale_max = 5
)

# Splicing the input list into calls to the the SPRITE and CLOSURE generators
subset <- rlang::inject(sprite_generate(!!!inputs_constant))
superset <- rlang::inject(closure_generate(!!!inputs_constant))

test_that("The subset/superset example data have the right shape (constant input)", {
  subset |> check_generator_output("SPRITE") |> expect_no_error()
  superset |> check_generator_output("CLOSURE") |> expect_no_error()
})

test_that("The subset/superset example data recompute correctly (constant input)", {
  subset |> count_wrong_stats() |> any_wrong_stats() |> expect_false()
  superset |> count_wrong_stats() |> any_wrong_stats() |> expect_false()
})

test_that("SPRITE results are a subset of CLOSURE results (constant input)", {
  subset |> is_contained_in(superset) |> expect_true()
})

if (plot_bar_both && !is_empty(subset) && !is_empty(superset)) {
  sprite_plot_bar(subset)
  closure_plot_bar(superset)
}


endpoint <- 5

inputs_random <- list(
  mean = 1 |> runif(min = 1, max = endpoint) |> round(1) |> as.character(),
  sd = 1 |> rnorm(mean = 0.8, sd = 0.5) |> round(1) |> as.character(),
  n = 1 |> rnorm(mean = 25, sd = 10) |> round(),
  scale_min = 1,
  scale_max = endpoint
)

# Splice as above
subset <- rlang::inject(sprite_generate(!!!inputs_random))
superset <- rlang::inject(closure_generate(!!!inputs_random))

# fmt: skip
test_that("The subset/superset example data have the right shape (random input)", {
  subset |> check_generator_output("SPRITE", allow_empty = TRUE) |> expect_no_error()
  superset |> check_generator_output("CLOSURE", allow_empty = TRUE) |> expect_no_error()
})

test_that("The subset/superset example data recompute correctly (random input)", {
  subset |> count_wrong_stats() |> any_wrong_stats() |> expect_false()
  superset |> count_wrong_stats() |> any_wrong_stats() |> expect_false()
})

test_that("SPRITE results are a subset of CLOSURE results (random input)", {
  subset |> is_contained_in(superset) |> expect_true()
})

if (plot_bar_both && !is_empty(subset) && !is_empty(superset)) {
  sprite_plot_bar(subset)
  closure_plot_bar(superset)
}
