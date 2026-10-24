# Regression tests for bugs found in the 0.3.0 audit

test_that("asymmetric rounding methods search the correct interval", {
  # With `rounding = "floor"`, the true mean lies in [3.5, 3.6) and the true SD
  # in [1.0, 1.1). Before the fix, the symmetric rounding error was 0, so
  # CLOSURE found no samples at all.
  data <- closure_generate("3.5", "1.0", 30, 1, 5, items_averaged = 1, rounding = "floor")
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


test_that("`closure_generate()` rejects `items_averaged != 1` instead of ignoring it", {
  expect_error(closure_generate("3.5", "1.0", 30, 1, 5, items_averaged = 2), "items")
  expect_error(closure_generate("3.5", "1.0", 30.5, 1, 5, items_averaged = 1), "whole number")
})


test_that("negative scale values survive a round trip through disk", {
  path <- withr::local_tempdir()
  data <- closure_generate("-0.5", "1.2", 30, -3, 3, items_averaged = 1, path = path)
  data_all <- closure_read(data$directory$path, include = "all")

  expect_equal(data_all$metrics_main$samples_all, data$metrics_main$samples_all)
  expect_gt(nrow(data_all$results), 0)
})


test_that("empty results written to disk can be read back", {
  path <- withr::local_tempdir()
  data <- suppressWarnings(
    closure_generate("3.5", "0.0", 30, 1, 5, items_averaged = 1, path = path)
  )

  expect_true(is_empty(data))

  for (include in c("stats_only", "stats_and_horns", "all")) {
    data_read <- closure_read(data$directory$path, include = include)
    expect_true(is_empty(data_read))
  }
})


test_that("count column names round half away from zero, like closure-core", {
  # With 8 items, 1.125 is 112.5 hundredths: closure-core rounds it to 113
  expect_equal(count_col_names(c(1.125, 1.625, -1.125)), c("v1_13", "v1_63", "vn1_13"))

  data <- sprite_generate("3.0", "1.0", 20, 1, 5, items_averaged = 8, stop_after = 5)
  expect_equal(names(data$results)[2:3], c("v1", "v1_13"))
})


test_that("`items_averaged` is part of the inputs and the folder name", {
  data <- sprite_generate("3.0", "1.0", 20, 1, 5, items_averaged = 2, stop_after = 5)
  expect_equal(data$inputs$items_averaged, 2)
  expect_equal(closure_generate("3.5", "1.7", 30, 1, 5, items_averaged = 1)$inputs$items_averaged, 1)

  # The same statistics with different `items_averaged` no longer clash on disk
  path <- withr::local_tempdir()
  data_2 <- sprite_generate("3.0", "1.0", 20, 1, 5, items_averaged = 2, stop_after = 5, path = path)
  data_3 <- sprite_generate("3.0", "1.0", 20, 1, 5, items_averaged = 3, stop_after = 5, path = path)
  expect_false(identical(data_2$directory$path, data_3$directory$path))
  expect_equal(sprite_read(data_3$directory$path)$inputs$items_averaged, 3)
})


test_that("a one-point scale works", {
  data <- closure_generate("3", "0", 10, scale_min = 3, scale_max = 3, items_averaged = 1)
  expect_equal(data$metrics_main$samples_all, 1)
  expect_equal(horns(5, 3, 3), 0)
  expect_equal(horns_rescaled(5, 3, 3), 0)
})


test_that("`closure_count_all()` errors instead of returning error text", {
  expect_error(closure_count_all("3.0", "1.0", 1, 1, 5), "n must be at least 2")
})


test_that("non-numeric `mean` or `sd` strings get a clear error", {
  expect_error(closure_generate("abc", "1.0", 10, 1, 5, items_averaged = 1), "`mean` must be a number")
  expect_error(closure_generate("3.0", "", 10, 1, 5, items_averaged = 1), "`sd` must be a number")
  expect_error(closure_count_all("3.0", "x", 10, 1, 5), "`sd` must be a number")
})


test_that("`stop_after` must be at least 1", {
  expect_error(closure_generate("3.5", "1.7", 30, 1, 5, items_averaged = 1, stop_after = 0), "at least 1")
})


test_that("`facet_labels_parens = NULL` removes the parentheses", {
  data <- closure_generate("3.5", "1.7", 30, 1, 5, items_averaged = 1)
  facet_labels <- function(plot) {
    plot$facet$params$labeller(data.frame(samples = factor(1:2)))$samples
  }

  expect_equal(
    facet_labels(closure_plot_bar(data, facet_labels_parens = NULL)),
    c("Minimal variance", "Maximal variance")
  )
  expect_equal(facet_labels(closure_plot_bar(data, facet_labels = NULL)), c("", ""))
})


test_that("`samples = \"all\"` in bar plots shows total counts", {
  data <- closure_generate("3.5", "1.7", 30, 1, 5, items_averaged = 1)
  plot_mean <- closure_plot_bar(data, format = "absolute", overlay = "none")
  plot_all <- closure_plot_bar(data, format = "absolute", samples = "all")

  horns <- data$results$horns
  group_sizes <- c(
    sum(abs(horns - data$metrics_horns$min) < 1e-10),
    sum(abs(horns - data$metrics_horns$max) < 1e-10)
  )
  expect_equal(
    plot_all$data$frequency,
    plot_mean$data$frequency * rep(group_sizes, each = 5)
  )

  # Each group's total is the number of values in all of its samples
  totals <- tapply(plot_all$data$frequency, plot_all$data$samples, sum)
  expect_equal(as.vector(totals), group_sizes * 30)

  # Without horns values, totals can't be computed
  path <- withr::local_tempdir()
  data_stats <- closure_read(closure_write(data, path))
  expect_error(closure_plot_bar(data_stats, samples = "all"), "horns index")
})


test_that("results can be read from a path with backslashes on Windows", {
  skip_on_os(c("mac", "linux", "solaris"))
  path <- withr::local_tempdir()
  data <- closure_generate("3.5", "1.7", 30, 1, 5, items_averaged = 1, path = path)
  path_backslashes <- gsub("/", "\\\\", data$directory$path)
  expect_no_error(closure_read(path_backslashes))
})


test_that("`items_averaged` must be specified and be at least 1", {
  expect_error(closure_generate("3.5", "1.7", 30, 1, 5), "Need to specify `items_averaged`")
  expect_error(sprite_generate("3.5", "1.7", 30, 1, 5, stop_after = 5), "Need to specify `items_averaged`")
  expect_error(sprite_generate("3.5", "1.7", 30, 1, 5, items_averaged = 0, stop_after = 5), "at least 1")
})
