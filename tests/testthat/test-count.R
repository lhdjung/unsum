# Whether a vector of counts rises to a maximum and then falls, without a dip
is_single_peaked <- function(counts) {
  falling <- FALSE
  for (step in diff(counts)) {
    if (step > 0 && falling) {
      return(FALSE)
    }
    if (step < 0) {
      falling <- TRUE
    }
  }
  TRUE
}

# The output of `closure_count_details()`, computed from the samples that
# `closure_generate()` lists
details_by_listing <- function(...) {
  data <- closure_generate(..., items_averaged = 1)
  counts <- data$results[setdiff(names(data$results), c("id", "horns"))] |>
    as.matrix()
  single_peak <- apply(counts, 1L, is_single_peaked)
  values <- data$modality_counts$value

  ranges <- function(rows) {
    if (!any(rows)) {
      na <- rep(NA_integer_, length(values))
      return(list(lo = na, hi = na, mean = rep(NA_real_, length(values))))
    }
    sub <- counts[rows, , drop = FALSE]
    list(
      lo = unname(apply(sub, 2L, min)),
      hi = unname(apply(sub, 2L, max)),
      mean = unname(colMeans(sub))
    )
  }
  all <- ranges(rep(TRUE, nrow(counts)))
  sp <- ranges(single_peak)

  # One row per value and count that occurs in some sample
  dist <- seq_along(values) |>
    lapply(function(j) {
      present <- sort(unique(counts[, j]))
      tibble::tibble(
        value = values[j],
        count = present,
        n_samples = vapply(
          present,
          function(c) as.double(sum(counts[, j] == c)),
          double(1L)
        ),
        n_samples_single_peak = vapply(
          present,
          function(c) as.double(sum(counts[, j] == c & single_peak)),
          double(1L)
        )
      )
    }) |>
    do.call(what = rbind)

  list(
    metrics_main = tibble::tibble(
      samples_all = as.double(nrow(counts)),
      samples_single_peak = as.double(sum(single_peak))
    ),
    modality_counts = tibble::tibble(
      value = values,
      count_lo = all$lo,
      count_hi = all$hi,
      count_mean = all$mean,
      single_peak_count_lo = sp$lo,
      single_peak_count_hi = sp$hi,
      single_peak_count_mean = sp$mean
    ),
    frequency_dist = dist
  )
}

test_that("`closure_count_details()` agrees with the samples CLOSURE lists", {
  cases <- list(
    list(mean = "3.5", sd = "1.2", n = 40, scale_min = 1, scale_max = 5),
    list(mean = "2.9", sd = "0.5", n = 25, scale_min = 1, scale_max = 5),
    list(mean = "4.0", sd = "2.1", n = 30, scale_min = 1, scale_max = 7),
    list(mean = "3.0", sd = "1.9", n = 20, scale_min = 1, scale_max = 5),
    list(mean = "4.00", sd = "1.73", n = 5, scale_min = 1, scale_max = 7)
  )
  for (case in cases) {
    expected <- suppressMessages(rlang::exec(details_by_listing, !!!case))
    actual <- rlang::exec(closure_count_details, !!!case)
    expect_equal(actual, expected)
    expect_identical(
      actual$metrics_main$samples_all,
      as.double(rlang::exec(closure_count_all, !!!case))
    )
  }
})

test_that("`closure_count_details()` finds no single-peaked sample where none exists", {
  out <- closure_count_details(
    mean = "4.00",
    sd = "1.73",
    n = 5,
    scale_min = 1,
    scale_max = 7
  )
  expect_gt(out$metrics_main$samples_all, 0)
  expect_identical(out$metrics_main$samples_single_peak, 0)
  expect_true(all(is.na(out$modality_counts$single_peak_count_lo)))
  expect_true(all(out$frequency_dist$n_samples_single_peak == 0))
})

test_that("`closure_count_details()` rejects invalid input", {
  closure_count_details("3.0", "1.0", 1, 1, 5) |>
    expect_error("failed on the Rust level")
  closure_count_details("3.0", "1.0", 10, 1, 1) |>
    expect_error()
})

test_that("The shape cutoffs can be set", {
  data <- closure_generate(
    mean = "3.5",
    sd = "1.2",
    n = 40,
    scale_min = 1,
    scale_max = 5,
    items_averaged = 1,
    prominence = 0.1,
    prominence_band = c(0.05, 0.2)
  ) |>
    suppressMessages()
  expect_identical(data$modality_summary$min_prominence, 0.1)
  expect_identical(
    sort(unique(data$modality_prominence$min_prominence)),
    c(0.05, 0.1, 0.2)
  )

  closure_generate(
    mean = "3.5",
    sd = "1.2",
    n = 40,
    scale_min = 1,
    scale_max = 5,
    items_averaged = 1,
    prominence = 5
  ) |>
    expect_error("fractions of n")
  closure_generate(
    mean = "3.5",
    sd = "1.2",
    n = 40,
    scale_min = 1,
    scale_max = 5,
    items_averaged = 1,
    prominence_band = NA
  ) |>
    expect_error("prominence_band")
})

test_that("Printing says when no single-peaked sample has the statistics", {
  # Beyond the largest SD of any single-peaked sample of 5 values
  data <- closure_generate(
    mean = "4.00",
    sd = "1.73",
    n = 5,
    scale_min = 1,
    scale_max = 7,
    items_averaged = 1
  ) |>
    suppressMessages()
  expect_true(data$metrics_horns$beyond_single_peak)
  expect_message(print(data), "No single-peaked sample")

  data <- closure_generate(
    mean = "3.5",
    sd = "1.2",
    n = 40,
    scale_min = 1,
    scale_max = 5,
    items_averaged = 1
  ) |>
    suppressMessages()
  expect_false(data$metrics_horns$beyond_single_peak)
  expect_no_message(print(data), message = "No single-peaked sample")
})
