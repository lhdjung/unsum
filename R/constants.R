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
  "modality_shapes",
  "modality_summary",
  "modality_prominence",
  "frequency",
  "frequency_dist",
  "results"
)


# All possible combinations of tibble names in valid generator output, in the
# order in which the elements appear. The first two are in-memory forms; the
# last two are read back from disk, where `results` is absent if `include =
# "stats_only"`.
TIBBLE_NAMES_POSSIBLE_FORMS <- list(
  TIBBLE_NAMES,
  c(TIBBLE_NAMES, "directory"),
  c(TIBBLE_NAMES[TIBBLE_NAMES != "results"], "directory"),
  c(TIBBLE_NAMES[TIBBLE_NAMES != "results"], "directory", "results")
)

# Names of the files expected in a folder with unsum results written to disk.
# `modality_conclusion` is derived from modality_summary.parquet, `directory` is
# not persisted, and `results` is split across the last three files:
# counts.parquet holds one count column per scale value plus `horns`,
# scale_values.parquet maps those columns to scale values, and format.parquet
# describes the layout.
FILES_EXPECTED <- c(
  "info.md",
  "inputs.parquet",
  "metrics_main.parquet",
  "metrics_horns.parquet",
  "modality_counts.parquet",
  "modality_pairs.parquet",
  "modality_shapes.parquet",
  "modality_summary.parquet",
  "modality_prominence.parquet",
  "frequency.parquet",
  "frequency_dist.parquet",
  "counts.parquet",
  "scale_values.parquet",
  "format.parquet"
)

# Shape classes that closure-core sorts each sample into, in its order
SHAPE_CLASSES <- c(
  "flat",
  "one_mode_interior",
  "one_mode_low_edge",
  "one_mode_high_edge",
  "two_modes",
  "three_or_more_modes"
)

# Column names and types of the `modality_summary` tibble. The `n_*` columns
# count samples per shape class at the primary mode threshold; the `band_n_*`
# columns count those with that shape at any threshold in the prominence band.
MODALITY_SUMMARY_TYPES <- c(
  list(
    "exhaustive" = "logical",
    "n_scanned" = "double",
    "min_prominence" = "double",
    "deficit_min" = "integer",
    "deficit_mean" = "double",
    "deficit_max" = "integer"
  ),
  `names<-`(as.list(rep("double", 6L)), paste0("n_", SHAPE_CLASSES)),
  `names<-`(as.list(rep("double", 6L)), paste0("band_n_", SHAPE_CLASSES))
)
