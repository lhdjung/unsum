# Run tests/testthat/test-generate.R a given number of times
if (TRUE) {
  index_max <- 30
  index <- 1

  while (index < index_max) {
    cli::cli_alert_info("Starting run {index} out of {index_max}...")
    # test(filter = "generate", stop_on_failure = TRUE)
    # # To keep the test-failing objects in the environment:
    source("tests/testthat/test-generate.R")
    index <- index + 1
  }

  cat("\n")
  cli::cli_alert_success("All runs finished!")
}
