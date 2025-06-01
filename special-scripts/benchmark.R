
n <- 275

result <- bench::mark(
  data_low = {
    closure_generate(
      mean = "3.5",
      sd = "0.5",
      n = n,
      scale_min = 1,
      scale_max = 5
    )
  },
  # data_high = {
  #   closure_generate(
  #     mean = "3.5",
  #     sd = "2",
  #     n = 200,
  #     scale_min = 1,
  #     scale_max = 5
  #   )
  # },
  check = FALSE,
  iterations = 30
)


result |>
  print() |>
  plot()


path_base <- "/home/lukas/Documents/r_projects/packages/unsum/benchmark_"

# readr::write_csv(result, paste0(path_base, "master_direct_n", n, ".csv"))
readr::write_csv(result, paste0(path_base, "generic_direct_n", n, ".csv"))

df_master  <- readr::read_csv(paste0(path_base, "master_direct_n", n, ".csv"))
df_generic <- readr::read_csv(paste0(path_base, "generic_direct_n", n, ".csv"))

df_master
df_generic


# Read saved data frames from disk
read_benchmark <- function(name) {
  path_base |>
    stringr::str_remove("benchmark_$") |>
    paste0(name) |>
    readr::read_csv(show_col_types = FALSE)
}


read_benchmark(paste0("benchmark_generic_direct_n", n, ".csv"))
read_benchmark(paste0("benchmark_master_direct_n", n, ".csv"))

read_benchmark(paste0("benchmark_generic_n", n, ".csv"))
read_benchmark(paste0("benchmark_master_n", n, ".csv"))

read_benchmark(paste0("benchmark_generic_n", n, ".csv"))
read_benchmark(paste0("benchmark_master_n", n, ".csv"))








