# Helper: compute frequency distribution in R from $results$sample.
#
# For each scale value v and each sample, count how many times v appears.
# Then tabulate: for each (value, count) pair, how many samples yielded
# exactly that count? The result is a data frame with columns:
#   value     -- scale value (integer)
#   count     -- number of occurrences of `value` in one sample (0..n)
#   n_samples -- number of samples with this (value, count) pair
compute_frequency_dist_r <- function(data) {
  scale_vals <- seq(data$inputs$scale_min, data$inputs$scale_max)
  samples <- data$results$sample

  rows <- lapply(scale_vals, function(v) {
    counts <- vapply(samples, function(s) sum(s == v), integer(1))
    count_tbl <- table(counts)
    data.frame(
      value     = v,
      count     = as.integer(names(count_tbl)),
      n_samples = as.integer(count_tbl),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


# Sort a frequency-dist data frame by (value, count) for comparison
sort_frequency_dist <- function(fd) {
  fd[order(fd$value, fd$count), ]
}


# Generate a small, deterministic CLOSURE result set for all tests below
fd_data <- closure_generate(
  mean      = "3.5",
  sd        = "0.5",
  n         = 12,
  scale_min = 1,
  scale_max = 5
)

fd_r    <- compute_frequency_dist_r(fd_data)
fd_rust <- fd_data$frequency_dist

n_samples_total <- nrow(fd_data$results)
scale_vals      <- seq(fd_data$inputs$scale_min, fd_data$inputs$scale_max)
n               <- fd_data$inputs$n


# --- Structural tests (R-computed) ------------------------------------------

test_that("R-computed frequency_dist contains every scale value", {
  expect_true(all(scale_vals %in% fd_r$value))
})

test_that("R-computed frequency_dist has no values outside the scale", {
  expect_true(all(fd_r$value %in% scale_vals))
})

test_that("R-computed frequency_dist: counts are in 0..n", {
  expect_true(all(fd_r$count >= 0L))
  expect_true(all(fd_r$count <= n))
})

test_that("R-computed frequency_dist: n_samples are positive", {
  expect_true(all(fd_r$n_samples > 0L))
})

test_that("R-computed frequency_dist: n_samples sum to total per value", {
  # For each scale value, the samples that have that value *some* number of
  # times partition the full sample set; so sum(n_samples) == n_samples_total.
  sums_by_value <- tapply(fd_r$n_samples, fd_r$value, sum)
  expect_true(all(sums_by_value == n_samples_total))
})

test_that("R-computed frequency_dist: weighted count sum matches frequency table", {
  # sum(count * n_samples) for each scale value equals the total number of
  # times that value appears across all samples, i.e. f_absolute.
  weighted_sums <- tapply(
    fd_r$count * fd_r$n_samples,
    fd_r$value,
    sum
  )

  # $frequency has one row per (samples, value) group; restrict to "all" so
  # f_absolute is not summed across horns_min/horns_max groups as well
  freq_all <- fd_data$frequency[fd_data$frequency$samples == "all", ]
  freq_by_value <- tapply(
    freq_all$f_absolute,
    freq_all$value,
    sum
  )

  expect_equal(
    as.integer(weighted_sums[as.character(scale_vals)]),
    as.integer(freq_by_value[as.character(scale_vals)])
  )
})


# --- Comparison: R-computed vs. Rust-produced --------------------------------

test_that("$frequency_dist is present in closure_generate() output", {
  expect_true("frequency_dist" %in% names(fd_data))
  expect_s3_class(fd_rust, "data.frame")
  expect_named(fd_rust, c("value", "count", "n_samples"))
})

test_that("Rust frequency_dist has same (value, count) pairs as R computation", {
  r_sorted    <- sort_frequency_dist(fd_r)
  rust_sorted <- sort_frequency_dist(fd_rust)

  row.names(r_sorted)    <- NULL
  row.names(rust_sorted) <- NULL

  expect_equal(r_sorted$value, rust_sorted$value)
  expect_equal(r_sorted$count, rust_sorted$count)
})

test_that("Rust frequency_dist n_samples matches R computation exactly", {
  r_sorted    <- sort_frequency_dist(fd_r)
  rust_sorted <- sort_frequency_dist(fd_rust)

  row.names(r_sorted)    <- NULL
  row.names(rust_sorted) <- NULL

  expect_equal(r_sorted$n_samples, rust_sorted$n_samples)
})


# --- Repeat with a second, wider-scale case ----------------------------------

fd_data2 <- closure_generate(
  mean      = "4.0",
  sd        = "1.2",
  n         = 20,
  scale_min = 1,
  scale_max = 7
)

fd_r2    <- compute_frequency_dist_r(fd_data2)
fd_rust2 <- fd_data2$frequency_dist

test_that("Rust frequency_dist matches R computation (wider scale)", {
  r_sorted    <- sort_frequency_dist(fd_r2)
  rust_sorted <- sort_frequency_dist(fd_rust2)

  row.names(r_sorted)    <- NULL
  row.names(rust_sorted) <- NULL

  expect_equal(r_sorted$value,     rust_sorted$value)
  expect_equal(r_sorted$count,     rust_sorted$count)
  expect_equal(r_sorted$n_samples, rust_sorted$n_samples)
})
