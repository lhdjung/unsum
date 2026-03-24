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


test_that("f_count sums to n for each samples group", {
  freq_centered <- closure_generate(
    mean = "4.0",
    sd = "1.0",
    n = 50,
    scale_min = 1,
    scale_max = 7
  )$frequency

  # Each samples group's medoid is a single sample of size n=50
  for (grp in c("all", "horns_min", "horns_max")) {
    freq_centered[freq_centered$samples == grp, ]$f_count |>
      sum() |>
      expect_equal(50)
  }
})


# SPRITE ------------------------------------------------------------------

plot_bar_both <- TRUE

inputs_constant_closure <- list(
  mean = "3.5",
  sd = "1.7",
  n = 75,
  scale_min = 1,
  scale_max = 5
)

inputs_constant_sprite <- c(inputs_constant_closure, stop_after = 100)

# Splicing the input list into calls to the the SPRITE and CLOSURE generators
subset <- rlang::inject(sprite_generate(!!!inputs_constant_sprite))
superset <- rlang::inject(closure_generate(!!!inputs_constant_closure))

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
  subset |> sprite_plot_bar() |> print()
  superset |> closure_plot_bar() |> print()
}


endpoint <- 5

inputs_random_closure <- list(
  mean = 1 |>
    runif(min = 1, max = endpoint) |>
    round(1) |>
    as.character(),

  sd = 1 |>
    rnorm(mean = 0.8, sd = 0.5) |>
    round(1) |>
    max(0) |>
    as.character(),

  n = 1 |>
    rnorm(mean = 25, sd = 10) |>
    round(),

  scale_min = 1,
  scale_max = endpoint
)

inputs_random_sprite <- c(inputs_random_closure, stop_after = 100)

# Splice as above
subset <- rlang::inject(sprite_generate(!!!inputs_random_sprite))
superset <- rlang::inject(closure_generate(!!!inputs_random_closure))

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
  subset |> sprite_plot_bar() |> print()
  superset |> closure_plot_bar() |> print()
}
