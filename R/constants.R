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


# All possible combinations of tibble names in valid generator output, in the
# order in which the elements appear. The first three are in-memory forms (with
# the modality_* tibbles); the last two are read back from disk, where the
# modality_* tibbles are not persisted, and `results` is absent if
# `include = "stats_only"`.
TIBBLE_NAMES_READ <- c(
  "inputs",
  "metrics_main",
  "metrics_horns",
  "frequency",
  "frequency_dist",
  "directory"
)

TIBBLE_NAMES_POSSIBLE_FORMS <- list(
  TIBBLE_NAMES,
  c(TIBBLE_NAMES, "directory"),
  c(TIBBLE_NAMES[TIBBLE_NAMES != "results"], "directory"),
  TIBBLE_NAMES_READ,
  c(TIBBLE_NAMES_READ, "results")
)

# Names of the files expected in a folder with unsum results written to disk.
# The modality_* tibbles and `directory` are not persisted; see
# `TIBBLE_NAMES_POSSIBLE_FORMS` above.
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
