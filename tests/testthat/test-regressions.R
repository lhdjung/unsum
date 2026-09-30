# Regression tests for bugs found in the 0.3.0 audit

test_that("asymmetric rounding methods search the correct interval", {
  # With `rounding = "floor"`, the true mean lies in [3.5, 3.6) and the true SD
  # in [1.0, 1.1). Before the fix, the symmetric rounding error was 0, so
  # CLOSURE found no samples at all.
  data <- closure_generate("3.5", "1.0", 30, 1, 5, rounding = "floor")
  samples <- results_samples(data)
  means <- vapply(samples, mean, numeric(1))
  sds <- vapply(samples, sd, numeric(1))

  expect_gt(nrow(data$results), 0)
  expect_true(all(means >= 3.5 & means <= 3.6))
  expect_true(all(sds >= 1.0 & sds <= 1.1))
  expect_equal(
    closure_count_all("3.5", "1.0", 30, 1, 5, rounding = "floor"),
    nrow(data$results)
  )
})


test_that("`closure_generate()` rejects `items != 1` instead of ignoring it", {
  expect_error(closure_generate("3.5", "1.0", 30, 1, 5, items = 2), "items")
  expect_error(closure_generate("3.5", "1.0", 30.5, 1, 5), "whole number")
})


test_that("negative scale values survive a round trip through disk", {
  path <- withr::local_tempdir()
  data <- closure_generate("-0.5", "1.2", 30, -3, 3, path = path)
  data_all <- closure_read(data$directory$path, include = "all")

  expect_equal(data_all$metrics_main$samples_all, data$metrics_main$samples_all)
  expect_gt(nrow(data_all$results), 0)
})


test_that("empty results written to disk can be read back", {
  path <- withr::local_tempdir()
  data <- suppressWarnings(
    closure_generate("3.5", "0.0", 30, 1, 5, path = path)
  )

  expect_true(is_empty(data))

  for (include in c("stats_only", "stats_and_horns", "all")) {
    data_read <- closure_read(data$directory$path, include = include)
    expect_true(is_empty(data_read))
  }
})
