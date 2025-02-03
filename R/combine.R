
#' Create CLOSURE combinations
#'
#' @description Call `closure_combine()` to run the CLOSURE algorithm on a given
#'   set of summary statistics.
#'
#'   This can take seconds, minutes, or longer, depending on the input. Wide
#'   variance and large samples often lead to many combinations, i.e., long
#'   runtimes. These effects interact dynamically. For example, with large `n`,
#'   even very small increases in `sd` can greatly increase runtime and number
#'   of values found.
#'
#'   If the inputs are mutually inconsistent, there is a warning and an empty
#'   data frame.
#'
#' @param mean String (length 1). Reported mean.
#' @param sd String (length 1). Reported sample standard deviation.
#' @param n Numeric (length 1). Reported sample size.
#' @param scale_min,scale_max Numeric (length 1 each). Minimal and maximal
#'   possible values of the measurement scale (not the *empirical* min and
#'   max!). For example, with a 1-7 Likert scale, use `scale_min = 1` and
#'   `scale_max = 7`.
#' @param rounding String (length 1). Rounding method assumed to have created
#'   `mean` and `sd`. See [*Rounding
#'   options*](https://lhdjung.github.io/roundwork/articles/rounding-options.html),
#'   but also the *Rounding limitations* section below. Default is
#'   `"up_or_down"` which, e.g., unrounds `0.12` to `0.115` as a lower bound and
#'   `0.125` as an upper bound.
#' @param threshold Numeric (length 1). Number from which to round up or down,
#'   if `rounding` is any of `"up_or_down"`, `"up"`, and `"down"`. Default is
#'   `5`.
#' @param warn_if_empty Logical (length 1). Should a warning be shown if no
#'   combinations are found? Default is `TRUE`.
#'
#' @section Rounding limitations: The `rounding` and `threshold` arguments are
#'   not fully implemented. For example, CLOSURE currently treats all rounding
#'   bounds as inclusive, even if the `rounding` specification would imply
#'   otherwise.
#'
#'   Many specifications of the two arguments will not make any difference, and
#'   those that do will most likely lead to empty results.
#'
#' @return Named list with these elements:
#'   - **`metadata`**: Tibble (data frame) with the inputs and these additional
#'   columns:
#'     - `combos_initial`: integer. The basis for computing CLOSURE results,
#'   based on scale range only.
#'     - `combos_all`: integer. Number of all combinations. Equal to the number
#'   of rows in `results`.
#'     - `values_all`: integer. Number of all individual values found. Equal to
#'   `n * combos_all`.
#'   - **`frequency`**: Tibble with these columns:
#'     - `value`: integer. Scale values derived from `scale_min` and
#'   `scale_max`.
#'     - `f_absolute`: integer. Count of individual scale values found in the
#'   `results` combinations.
#'     - `f_relative`: double. Values' share of total values found.
#'   - **`results`**: Tibble with these columns:
#'     - `id`: integer. Runs from `1` to `combos_all`.
#'     - `combination`: list of integer vectors. Each of these vectors has
#'   length `n`. It is a combination (or distribution) of individual scale
#'   values found by CLOSURE.
#'
#' @include utils.R count.R extendr-wrappers.R
#'
#' @export
#'
#' @examples
#' # High spread often leads to many combinations --
#' # here, 735.
#' data_high <- closure_combine(
#'   mean = "3.5",
#'   sd = "2",
#'   n = 52,
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
#' # Low spread, only 3 combinations, and not all
#' # scale values are possible.
#' data_low <- closure_combine(
#'   mean = "3.5",
#'   sd = "0.5",
#'   n = 52,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' data_low
#'
#' # This can also be shown by `closure_plot_bar()`:
#' closure_plot_bar(data_low)


# Note: most helper functions called here can be found in the R/utils.R file.
# The only exception, `create_combinations()`, is in R/extendr-wrappers.R, but
# all it does is to call into Rust code in scr/rust/src/lib.rs which, in turn,
# accesses closure-core. The latter is a Rust crate (roughly analogous to an R
# package) that contains the actual implementation of CLOSURE:
# https://github.com/lhdjung/closure-core/blob/master/src/lib.rs


# # For interactive testing:
# mean <- "5.00"
# sd <- "2.78"
# n <- 30
# rounding = "up_or_down"
# threshold = 5
# scale_min <- 1
# scale_max <- 8
# warn_if_empty <- TRUE
# rounding_error_mean <- NULL
# rounding_error_sd <- NULL


closure_combine <- function(mean,
                            sd,
                            n,
                            scale_min,
                            scale_max,
                            rounding = "up_or_down",
                            threshold = 5,
                            warn_if_empty = TRUE,
                            rounding_error_mean = NULL,
                            rounding_error_sd = NULL) {

  # Comprehensive checks make sure that each argument is of the right type, has
  # length 1, and is not `NA`.
  check_value(mean, "character")
  check_value(sd, "character")
  check_value(n, c("double", "integer"))
  check_value(scale_min, c("double", "integer"))
  check_value(scale_max, c("double", "integer"))
  check_value(warn_if_empty, "logical")

  check_scale(scale_min, scale_max, as.numeric(mean))

  mean_num <- as.numeric(mean)
  sd_num   <- as.numeric(sd)

  # TODO: Maybe take `scale_min` and `scale_max` into account; they might
  # further confine the results of `unround()`!

  # Reconstruct the min and max possible values of the unknown original number
  # that was later rounded to the reported mean and SD values. Subtract each
  # lower bound (i.e., each minimum) from the respective reported value to
  # compute the rounding error.
  mean_sd_unrounded <- roundwork::unround(
    x = c(mean, sd),
    rounding = rounding,
    threshold = threshold
  )

  if (is.null(rounding_error_mean)) {
    rounding_error_mean <- mean_num - mean_sd_unrounded$lower[1]
  }

  if (is.null(rounding_error_sd)) {
    rounding_error_sd   <- sd_num   - mean_sd_unrounded$lower[2]
  }

  # Compute CLOSURE combinations by calling into pre-compiled Rust code.
  results <- create_combinations(
    mean = mean_num,
    sd = sd_num,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    rounding_error_mean = rounding_error_mean,
    rounding_error_sd = rounding_error_sd
  )

  n_combos_all <- length(results)

  # By default, raise a warning if no results were found.
  if (warn_if_empty && n_combos_all == 0L) {
    cli::cli_warn(c(
      "No results found with these inputs.",
      "x" = "Data internally inconsistent.",
      "x" = "These statistics can't describe the same distribution."
    ))
  }

  # Insert the combinations into a data frame, along with summary statistics.
  # The S3 class "closure_combine" will be recognized by downstream functions,
  # such as `closure_plot_bar()`. Two elements here are created using the
  # low-level `new_tibble()` instead of `tibble()`: once for passing the S3
  # class, and once for performance.
  list(
    metadata = tibble::new_tibble(
      x = list(
        mean = mean,
        sd = sd,
        n = n,
        scale_min = scale_min,
        scale_max = scale_max,
        combos_initial = closure_count_initial(scale_min, scale_max),
        combos_all = n_combos_all,
        values_all = n_combos_all * as.integer(n)
      ),
      nrow = 1L,
      class = "closure_combine"
    ),
    frequency = summarize_frequencies(results, scale_min, scale_max),
    results = tibble::new_tibble(
      x = list(
        id = seq_len(n_combos_all),
        combination = results
      ),
      nrow = n_combos_all
    )
  )

}

