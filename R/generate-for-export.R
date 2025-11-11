#' Generate CLOSURE samples
#'
#' @description Call `closure_generate()` to run the CLOSURE algorithm on a
#'   given set of summary statistics.
#'
#'   This can take seconds, minutes, or longer, depending on the input. Wide
#'   variance and large `n` often lead to many samples, i.e., long runtimes.
#'   These effects interact dynamically. For example, with large `n`, even very
#'   small increases in `sd` can greatly increase runtime and number of values
#'   found. Consider specifying `path` in these cases; see "Writing to disk"
#'   below.
#'
#'   If the inputs are inconsistent, there is no solution. The function will
#'   then return empty results and throw a warning.
#'
#' @param mean String (length 1). Reported mean.
#' @param sd String (length 1). Reported sample standard deviation.
#' @param n Numeric (length 1). Reported sample size.
#' @param scale_min,scale_max Numeric (length 1 each). Minimal and maximal
#'   possible values. For example, with a 1-7 Likert scale, use `scale_min = 1`
#'   and `scale_max = 7`. Prefer the empirical min and max if available: they
#'   constrain the possible values further.
#' @param path String (length 1). Optionally, choose the directory where a new
#'   folder with CLOSURE results should be created. Use `path = "."` for your
#'   current working directory. See "Writing to disk" below.
#' @param stop_after Numeric (length 1). Optionally, make CLOSURE stop searching
#'   after it found this many samples. Useful if the goal is just to check
#'   whether the inputs are consistent: e.g., with `stop_after = 1`, the
#'   function returns either one sample or none. The latter would indicate an
#'   inconsistency.
#' @param include String (length 1). If results are written to disk, which parts
#'   of them should be included in the R output?
#'   - With `"stats_and_horns"`, the default, all parts except for the samples
#'   are included.
#'   - `"stats_only"` excludes the `"results"` tibble, i.e., samples and horns.
#'   - `"all"` reads the full results, including the samples and horns values.
#' @param rounding String (length 1). Rounding method assumed to have created
#'   `mean` and `sd`. See [*Rounding
#'   options*](https://lhdjung.github.io/roundwork/articles/rounding-options.html),
#'   but also the *Rounding limitations* section below. Default is
#'   `"up_or_down"` which, e.g., unrounds `0.12` to `0.115` as a lower bound and
#'   `0.125` as an upper bound.
#' @param threshold Numeric (length 1). Number from which to round up or down,
#'   if `rounding` is any of `"up_or_down"`, `"up"`, and `"down"`. Default is
#'   `5`.
#' @param ask_to_proceed Logical (length 1). If the runtime is predicted to be
#'   very long in an interactive setting, should the function prompt you to
#'   proceed or abort? Default is `TRUE`.
#'
#' @section Writing to disk: Specify `path` if the expected runtime is very
#'   long. (In case you have trouble choosing a path, use `path = "."` for your
#'   current working directory.) This makes sure the results are preserved by
#'   incrementally writing them to disk. Otherwise, you might encounter an
#'   out-of-memory error because `closure_generate()` accumulates more data than
#'   your computer can hold in memory.
#'
#'   If `path` is specified, the output in R will not include the `"sample"`
#'   column in the `results` tibble by default; but it will feature a
#'   `directory` tibble with the file path. If you choose to load the samples
#'   using `include = "all"`, you incur the risk of a memory error. Even so, the
#'   results are safe because they are written to disk first; and you can later
#'   try to load the full detailed results from disk using [`closure_read()`]
#'   and its `include` argument. This would separate saving the data to disk
#'   from an uncertain attempt to load them into R.
#'
#' @section More about memory: Some output columns that contain counts, such as
#'   `f_absolute`, are doubles instead of integers. This is because doubles are
#'   able to contain much larger numbers. When counting CLOSURE results, it is
#'   not unrealistic to reach the limit of 32-bit integers in R, which is
#'   roughly two billion.
#'
#' @section Rounding limitations: The `rounding` and `threshold` arguments are
#'   not fully implemented. For example, CLOSURE currently treats all rounding
#'   bounds as inclusive, even if the `rounding` value would imply otherwise.
#'   Many specifications of the two arguments will not make any difference, and
#'   those that do will most likely lead to empty results.
#'
#' @return `closure_generate()` returns a named list of tibbles (data frames):
#'   - **`inputs`**: Arguments to this function.
#'   - **`metrics_main`**:
#'     - `samples_initial`: integer. The basis for computing CLOSURE results,
#'   based on scale range only. See [`closure_count_initial()`].
#'     - `samples_all`: double. Number of all samples. Equal to the number
#'   of rows in `results`.
#'     - `values_all`: double. Number of all individual values found. Equal to
#'   `n * samples_all`.
#'   - **`metrics_horns`**:
#'     - `mean`: double. Average horns value of all samples. The horns index is
#'   a measure of dispersion for bounded scales; see [`horns()`].
#'     - `uniform`: double. The value that `mean` would have if all samples were
#'   uniformly distributed; see [`horns_uniform()`].
#'     - `sd`, `cv`, `mad`, `min`, `median`, `max`, `range`: double. Standard
#'   deviation, coefficient of variation, median absolute deviation, minimum,
#'   median, maximum, and range of the horns index values across all samples.
#'   Note that `mad` is not scaled using a constant, as [`stats::mad()`] is by
#'   default.
#'   - **`frequency`**:
#'     - `samples`: string. Frequencies apply to one of three subsets of
#'   samples: `"all"` for all samples, `"horns_min"` for those samples with the
#'   lowest horns index among all samples, and `"horns_max"` for those samples
#'   with the highest horns index.
#'     - `value`: integer. Scale values derived from `scale_min` and
#'   `scale_max`.
#'     - `f_average`: double. Count of scale values in the mean `results`
#'   sample.
#'     - `f_absolute`: double. Count of individual scale values found in the
#'   `results` samples.
#'     - `f_relative`: double. Values' share of total values found.
#'   - **`results`**:
#'     - `id`: integer. Runs from `1` to `samples_all`.
#'     - `sample` (not present by default if `path` was specified): list of
#'   integer vectors. Each of these vectors has length `n`. It is a sample (or
#'   distribution) of individual scale values found by CLOSURE.
#'     - `horns`: double. Horns index of each sample.
#'   - **`directory`** (only present if `path` was specified):
#'     - `path`: string. Location of the folder in which the results were saved.
#'
#' @include utils.R count.R horns.R performance.R read-write.R fn-factory.R
#'   extendr-wrappers.R
#'
#' @export
#'
#' @examples
#' # High spread often leads to many samples --
#' # here, 2492.
#' data_high <- closure_generate(
#'   mean = "3.5",
#'   sd = "1.7",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' data_high
#'
#' # Get a clear picture of the distribution
#' # by following up with `closure_plot_bar()`:
#' closure_plot_bar(data_high)
#'
#' # Low spread, only 4 samples, and not all
#' # scale values are possible.
#' data_low <- closure_generate(
#'   mean = "2.9",
#'   sd = "0.5",
#'   n = 25,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' data_low
#'
#' # This can also be shown by `closure_plot_bar()`:
#' closure_plot_bar(data_low)

# This constructs a function that wraps `generate_from_mean_sd_n()`; see there
closure_generate <- new_generator_mean_sd_n("CLOSURE")
