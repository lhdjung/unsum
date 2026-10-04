# Generate SPRITE samples

Call `sprite_generate()` to find possible samples using SPRITE (sample
parameter reconstruction via iterative techniques). SPRITE reconstructs
possible sample distributions given summary statistics and multi-item
scale information.

`stop_after` is required when `items > 1` to prevent overflow errors in
the current implementation.

## Usage

``` r
sprite_generate(
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  items = 1,
  path = NULL,
  stop_after = NULL,
  include = c("stats_and_horns", "stats_only", "all"),
  rounding = "up_or_down",
  threshold = 5,
  ask_to_proceed = TRUE
)
```

## Arguments

- mean:

  String (length 1). Reported mean.

- sd:

  String (length 1). Reported sample standard deviation.

- n:

  Numeric (length 1). Reported sample size.

- scale_min, scale_max:

  Numeric (length 1 each). Minimal and maximal possible values. For
  example, with a 1-7 Likert scale, use `scale_min = 1` and
  `scale_max = 7`. Prefer the empirical min and max if available: they
  constrain the possible values further.

- items:

  Numeric (length 1). Number of items/questions in your scale. Must be
  at least 2. This represents how many individual items were averaged to
  produce each participant's mean score.

- path:

  String (length 1). Optionally, choose the directory where a new folder
  with CLOSURE results should be created. Use `path = "."` for your
  current working directory. See "Writing to disk" below.

- stop_after:

  Numeric (length 1). **Required** for SPRITE when `items > 1`. Limits
  the number of samples returned to prevent overflow. Recommended value:
  100-1000 depending on your needs.

- include:

  String (length 1). If results are written to disk, which parts of them
  should be included in the R output?

  - With `"stats_and_horns"`, the default, all parts except for the
    samples are included.

  - `"stats_only"` excludes the `"results"` tibble, i.e., samples and
    horns.

  - `"all"` reads the full results, including the samples and horns
    values.

- rounding:

  String (length 1). Rounding method assumed to have created `mean` and
  `sd`. See [*Rounding
  options*](https://lhdjung.github.io/roundwork/articles/rounding-options.html),
  but also the *Rounding limitations* section below. Default is
  `"up_or_down"` which, e.g., unrounds `0.12` to `0.115` as a lower
  bound and `0.125` as an upper bound.

- threshold:

  Numeric (length 1). Number from which to round up or down, if
  `rounding` is any of `"up_or_down"`, `"up"`, and `"down"`. Default is
  `5`.

- ask_to_proceed:

  Logical (length 1). If the runtime is predicted to be very long in an
  interactive setting, should the function prompt you to proceed or
  abort? Default is `TRUE`.

## Value

`sprite_generate()` returns a named list of tibbles (data frames):

- **`inputs`**: Arguments to this function.

- **`metrics_main`**:

  - `samples_all`: double. Number of all samples. Equal to the number of
    rows in `results`.

  - `values_all`: double. Number of all individual values found. Equal
    to `n * samples_all`.

- **`metrics_horns`**:

  - `mean`: double. Average horns value of all samples. The horns index
    is a measure of dispersion for bounded scales; see
    [`horns()`](https://lhdjung.github.io/unsum/reference/horns.md).

  - `uniform`: double. The value that `mean` would have if all samples
    were uniformly distributed; see
    [`horns_uniform()`](https://lhdjung.github.io/unsum/reference/horns.md).

  - `sd`, `cv`, `mad`, `min`, `median`, `max`, `range`: double. Standard
    deviation, coefficient of variation, median absolute deviation,
    minimum, median, maximum, and range of the horns index values across
    all samples. Note that `mad` is not scaled using a constant, as
    [`stats::mad()`](https://rdrr.io/r/stats/mad.html) is by default.

- **`frequency`**:

  - `samples`: string. Frequencies apply to one of three subsets of
    samples: `"all"` for all samples, `"horns_min"` for those samples
    with the lowest horns index among all samples, and `"horns_max"` for
    those samples with the highest horns index.

  - `value`: double. Scale values derived from `scale_min` and
    `scale_max`.

  - `f_expected`: double. Average count of each scale value across the
    group's samples.

  - `f_representative`: double. Count of each scale value in the group's
    medoid, i.e., the actual sample with the smallest total distance
    (EMD) to all others. `NaN` if `path` was specified because the
    medoid can't be found while streaming results to disk.

  - `f_relative`: double. `f_expected` divided by `n`.

- **`modality_counts`**, **`modality_pairs`**, **`modality_shapes`**,
  **`modality_summary`**, **`modality_prominence`**: the range of counts
  of each scale value, and the shapes (e.g., one or two modes) that the
  samples take.

- **`modality_conclusion`**: whether any sample can be unimodal,
  bimodal, or J-shaped. `NA` means that no such sample was found, but
  the search was partial, so it might still exist. This is always the
  case with SPRITE, and with CLOSURE if `stop_after` was specified.

- **`results`**:

  - `id`: double. Runs from `1` to `samples_all`.

  - `v1`, `v2`, etc. (not present by default if `path` was specified):
    integer. One column per scale value, holding the number of times
    this value occurs in the sample. For instance, `v3` counts the 3s.
    Negative values are marked with `n`, as in `vn2` for -2, and decimal
    values with `_`, as in `v1_5` for 1.5 (only for SPRITE with
    `items > 1`). Together, these columns describe each sample
    completely because the order of values within a sample doesn't
    matter. The counts of each row sum up to `n`.

  - `horns`: double. Horns index of each sample.

- **`directory`** (only present if `path` was specified):

  - `path`: string. Location of the folder in which the results were
    saved.

## Writing to disk

Specify `path` if the expected runtime is very long. (In case you have
trouble choosing a path, use `path = "."` for your current working
directory.) This makes sure the results are preserved by incrementally
writing them to disk. Otherwise, you might encounter an out-of-memory
error because `sprite_generate()` accumulates more data than your
computer can hold in memory.

## More about memory

Some output columns that contain counts, such as `f_expected`, are
doubles instead of integers. This is because doubles are able to contain
much larger numbers. When counting SPRITE results, it is possible to
exceed the limit of 32-bit integers in R, which is roughly two billion.

## Rounding limitations

The `rounding` and `threshold` arguments are not fully implemented. For
example, SPRITE currently treats all rounding bounds as inclusive, even
if the `rounding` value would imply otherwise. Many specifications of
the two arguments will not make any difference, and those that do will
most likely lead to empty results.

## Printing

When printing results, you can use
[`print()`](https://rdrr.io/r/base/print.html) explicitly with the
`show` argument to control which elements are shown. Set `show` to one
of:

- `"some"` (the default): show some elements, hide others. The hidden
  ones have brief descriptions.

- `"all"`: show all elements.

- `"none"`: show no elements, only their descriptions.

For example: `print(your_results, show = "all")`

## Examples

``` r
# Basic example with 2 items
if (FALSE) { # \dontrun{
data_simple <- sprite_generate(
  mean = "3.0",
  sd = "1.0",
  n = 120,
  scale_min = 1,
  scale_max = 5,
  stop_after = 150
)

# Larger example - use stop_after to avoid overflow
sprite_generate(
  mean = "3.5",
  sd = "1.7",
  n = 1000,
  items = 5,
  scale_min = 1,
  scale_max = 5,
  stop_after = 1000
)
} # }
```
