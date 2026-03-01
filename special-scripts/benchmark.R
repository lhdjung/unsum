# Should be about 30 seconds
tictoc::tic()
closure_generate(
  mean = "3.1",
  sd = "2.1",
  n = 60,
  scale_min = 1,
  scale_max = 10,
  ask_to_proceed = FALSE
)
tictoc::toc()
