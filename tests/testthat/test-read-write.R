# Enable the test file to be run multiple times per session
temp_folder <- file.path(tempdir(), "unsum-test-read-write")
dir.create(temp_folder, showWarnings = FALSE)
withr::defer(unlink(temp_folder, recursive = TRUE), teardown_env())

path_closure <- closure_generate(
  mean = "3.5",
  sd = "1.7",
  n = 70,
  scale_min = 1,
  scale_max = 5,
  path = temp_folder
)$directory$path


test_that("`closure_read()` works", {
  path_closure |>
    closure_read() |>
    check_generator_output("CLOSURE") |>
    expect_no_error()
})


# Order of the rows in `results`, ignoring the ID, so that results found in
# different orders can be compared
sort_results <- function(results) {
  results <- results[-1L]
  results[do.call(order, unname(as.list(results))), ]
}

inputs_round_trip <- list(mean = "3.5", sd = "1.7", n = 30, scale_min = 1, scale_max = 5)
data_memory <- rlang::inject(closure_generate(!!!inputs_round_trip))

test_that("results streamed to disk are the same as those in memory", {
  path <- withr::local_tempdir()
  data_disk <- rlang::inject(closure_generate(!!!inputs_round_trip, path = path))
  data_disk <- closure_read(data_disk$directory$path, include = "all")

  expect_equal(
    sort_results(data_disk$results),
    sort_results(data_memory$results),
    ignore_attr = TRUE
  )
  for (name in c("metrics_main", "frequency_dist", "modality_conclusion", "modality_summary")) {
    expect_equal(data_disk[[name]], data_memory[[name]], ignore_attr = TRUE)
  }
  expect_equal(
    data_disk$frequency[c("samples", "value", "f_expected", "f_relative")],
    data_memory$frequency[c("samples", "value", "f_expected", "f_relative")],
    ignore_attr = TRUE
  )
})

test_that("`closure_write()` and `closure_read()` round-trip the results", {
  path <- withr::local_tempdir()
  data_read <- data_memory |>
    closure_write(path = path) |>
    closure_read(include = "all")

  expect_equal(data_read$results, data_memory$results, ignore_attr = TRUE)
  expect_setequal(dir(data_read$directory$path), FILES_EXPECTED)
})

test_that("multi-item SPRITE has count columns for fractional values", {
  data <- sprite_generate("3.5", "1.2", 20, 1, 5, items = 2, stop_after = 10)
  expect_equal(grid_values(data), seq(1, 5, by = 0.5))
  expect_equal(names(data$results)[2:4], c("v1", "v1_5", "v2"))
  expect_true(all(rowSums(results_counts(data)) == 20))
})
