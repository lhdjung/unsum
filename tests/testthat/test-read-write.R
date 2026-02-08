temp_folder <- tempdir()

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

unlink("path_closure", recursive = TRUE)
