# Note: This file contains objects that are created at build-time and not
# changed while functions run. Being constants, they have all-caps names.

# Names of the tibbles in the kind of list returned by `closure_generate()` etc.
# (i.e., by generator functions or "generators") by default
TIBBLE_NAMES <- c(
  "inputs",
  "metrics_main",
  "metrics_horns",
  "frequency",
  "results"
)

# Names of the files expected in a folder with unsum results written to disk
FILES_EXPECTED <- c(
  "info.md",
  "inputs.parquet",
  "metrics_main.parquet",
  "metrics_horns.parquet",
  "frequency.parquet",
  "horns.parquet",
  "sample.parquet"
)
