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
)@directory$path


test_that("`closure_read()` works", {
  path_closure |>
    closure_read() |>
    check_generator_output("CLOSURE") |>
    expect_no_error()
})
