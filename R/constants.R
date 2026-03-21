# Note: This file contains objects that are created at build-time and not
# changed while functions run. Being constants, they have all-caps names.

# Names of the tibbles in the kind of list returned by `closure_generate()` etc.
# (i.e., by generator functions or "generators") by default
TIBBLE_NAMES <- c(
  "inputs",
  "metrics_main",
  "metrics_horns",
  "modality_counts",
  "modality_pairs",
  "modality_conclusion",
  "frequency",
  "frequency_dist",
  "results"
)

# Tibble names for results read from disk: the three modality_* tibbles are not
# written to parquet, so disk reads return a list without them.
TIBBLE_NAMES_DISK <- TIBBLE_NAMES[!startsWith(TIBBLE_NAMES, "modality_")]

# All possible combinations of tibble names in valid generator output.
# Three in-memory forms (with modality_*) and three disk-read forms (without,
# since the modality_* tibbles are not persisted to parquet).
TIBBLE_NAMES_POSSIBLE_FORMS <- list(
  TIBBLE_NAMES,
  c(TIBBLE_NAMES, "directory"),
  c(TIBBLE_NAMES[TIBBLE_NAMES != "results"], "directory"),
  TIBBLE_NAMES_DISK,
  c(TIBBLE_NAMES_DISK, "directory"),
  c(TIBBLE_NAMES_DISK[TIBBLE_NAMES_DISK != "results"], "directory")
)

# Names of the files expected in a folder with unsum results written to disk
FILES_EXPECTED <- c(
  "info.md",
  "inputs.parquet",
  "metrics_main.parquet",
  "metrics_horns.parquet",
  "frequency.parquet",
  "frequency_dist.parquet",
  "horns.parquet",
  "sample.parquet"
)
