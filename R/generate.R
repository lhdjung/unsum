#' @include fn-factory.R
NULL

# CLOSURE -----------------------------------------------------------------

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
#' @return `r expand_section("generate_return", "CLOSURE")`
#'
#' @section `r expand_section("writing", "CLOSURE")`
#' @section `r expand_section("memory", "CLOSURE")`
#' @section `r expand_section("rounding", "CLOSURE")`
#'
#' @include utils.R count.R horns.R performance.R read-write-basic.R
#'   fn-factory.R extendr-wrappers.R
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


# SPRITE ------------------------------------------------------------------

#' Generate SPRITE samples
#'
#' @description Call `sprite_generate()` to find possible samples using SPRITE
#'   (sample parameter reconstruction via iterative techniques). SPRITE
#'   reconstructs possible sample distributions given summary statistics and
#'   multi-item scale information.
#'
#'   `stop_after` is required when `items > 1` to prevent overflow errors in the
#'   current implementation.
#'
#' @param items Numeric (length 1). Number of items/questions in your scale.
#'   Must be at least 2. This represents how many individual items were averaged
#'   to produce each participant's mean score.
#' @param stop_after Numeric (length 1). **Required** for SPRITE when `items >
#'   1`. Limits the number of samples returned to prevent overflow. Recommended
#'   value: 100-1000 depending on your needs.
#' @inheritParams closure_generate
#'
#' @return `r expand_section("generate_return", "SPRITE")`
#'
#' @section `r expand_section("writing", "SPRITE")`
#' @section `r expand_section("memory", "SPRITE")`
#' @section `r expand_section("rounding", "SPRITE")`
#'
#' @include doc-helpers.R
#' @export
#'
#' @examples
#' # Basic example with 2 items
#' \dontrun{
#' data_simple <- sprite_generate(
#'   mean = "3.0",
#'   sd = "1.0",
#'   n = 120,
#'   scale_min = 1,
#'   scale_max = 5,
#'   stop_after = 150
#' )
#'
#' # Larger example - use stop_after to avoid overflow
#' sprite_generate(
#'   mean = "3.5",
#'   sd = "1.7",
#'   n = 1000,
#'   items = 5,
#'   scale_min = 1,
#'   scale_max = 5,
#'   stop_after = 1000
#' )
#' }
#'
sprite_generate <- new_generator_mean_sd_n("SPRITE")
