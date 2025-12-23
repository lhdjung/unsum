generate_return <-
  "`closure_generate()` returns a named list of tibbles (data frames):
  - **`inputs`**: Arguments to this function.
  - **`metrics_main`**:
    - `samples_initial`: integer. The basis for computing CLOSURE results,
  based on scale range only. See [`closure_count_initial()`].
    - `samples_all`: double. Number of all samples. Equal to the number
  of rows in `results`.
    - `values_all`: double. Number of all individual values found. Equal to
  `n * samples_all`.
  - **`metrics_horns`**:
    - `mean`: double. Average horns value of all samples. The horns index is
  a measure of dispersion for bounded scales; see [`horns()`].
    - `uniform`: double. The value that `mean` would have if all samples were
  uniformly distributed; see [`horns_uniform()`].
    - `sd`, `cv`, `mad`, `min`, `median`, `max`, `range`: double. Standard
  deviation, coefficient of variation, median absolute deviation, minimum,
  median, maximum, and range of the horns index values across all samples.
  Note that `mad` is not scaled using a constant, as [`stats::mad()`] is by
  default.
  - **`frequency`**:
    - `samples`: string. Frequencies apply to one of three subsets of
  samples: `\"all\"` for all samples, `\"horns_min\"` for those samples with the
  lowest horns index among all samples, and `\"horns_max\"` for those samples
  with the highest horns index.
    - `value`: integer. Scale values derived from `scale_min` and
  `scale_max`.
    - `f_average`: double. Count of scale values in the mean `results`
  sample.
    - `f_absolute`: double. Count of individual scale values found in the
  `results` samples.
    - `f_relative`: double. Values' share of total values found.
  - **`results`**:
    - `id`: integer. Runs from `1` to `samples_all`.
    - `sample` (not present by default if `path` was specified): list of
  integer vectors. Each of these vectors has length `n`. It is a sample (or
  distribution) of individual scale values found by CLOSURE.
    - `horns`: double. Horns index of each sample.
  - **`directory`** (only present if `path` was specified):
    - `path`: string. Location of the folder in which the results were saved."
