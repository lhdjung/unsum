#' Count CLOSURE samples in advance
#'
#' @description Determine how many samples [`closure_generate()`] would find for
#'   a given set of summary statistics.
#'
#'   - `closure_count_all()` counts all CLOSURE samples that correspond to the
#'   input summary statistics, but without actually generating the samples. This
#'   is much faster than `closure_generate()` if there are many results.
#'   - `closure_count_initial()` only counts the first round of samples, from
#'   which all other ones would be generated. Based on scale range only.
#'
#'   This can help predict how much time [`closure_generate()`] would take, and
#'   avoid prohibitively long runs.
#'
#'   Both functions assume a single item, like `closure_generate()` with
#'   `items_averaged = 1`: each value in a sample is one of the integers from
#'   `scale_min` to `scale_max`. If `mean` and `sd` describe the average of
#'   several items per respondent, the counts are not meaningful.
#'
#' @inheritParams closure_generate
#' @param scale_min,scale_max Integers (length 1 each). Minimum and maximum of
#'   the scales to which the reported statistics refer.
#'
#' @return Integer (length 1).
#'
#' @include utils.R generate.R extendr-wrappers.R
#'
#' @export
#'
#' @examples
#' closure_count_all(
#'   mean = "3.5",
#'   sd = "1.7",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' closure_count_initial(scale_min = 1, scale_max = 5)

closure_count_all <- function(
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  rounding = "up_or_down",
  threshold = 5
) {
  call_count_fn(
    rust_fn = count_closure_combinations,
    mean = mean,
    sd = sd,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    rounding = rounding,
    threshold = threshold
  )
}


#' Count CLOSURE samples per scale value, and single-peaked ones
#'
#' @description `closure_count_details()` goes beyond [`closure_count_all()`]:
#'   for each scale value and each count, it says in how many CLOSURE samples
#'   the value occurs that many times. It does this over all samples and over
#'   the single-peaked ones, without generating any sample.
#'
#'   A sample is single-peaked if its counts rise to a maximum and then fall,
#'   with ties allowed but no dip at all. This needs no cutoff for what counts
#'   as a peak, unlike the shape classes of [`closure_generate()`].
#'
#'   The function settles whether any single-peaked sample has the reported
#'   statistics, and what the counts of every such sample would have to be. It
#'   also gives the range of each scale value's count across all samples, like
#'   `modality_counts` in the output of [`closure_generate()`]. All of this is
#'   exact, and it remains feasible at sample sizes where generating the
#'   samples is not. It takes about one and a half times as long as
#'   [`closure_count_all()`].
#'
#'   Like `closure_count_all()`, it assumes a single item per respondent.
#'
#' @inheritParams closure_count_all
#'
#' @return Named list of three tibbles:
#'   - **`metrics_main`** (one row): `samples_all`, the number of all samples,
#'   as returned by [`closure_count_all()`], and `samples_single_peak`, the
#'   number of single-peaked samples. Both are doubles.
#'   - **`modality_counts`** (one row per scale value): `value`, then
#'   `count_lo`, `count_hi`, and `count_mean`, the smallest, largest, and
#'   average count of the value across all samples; then the same over the
#'   single-peaked samples (`single_peak_count_lo` etc.). They are `NA` if
#'   there are no such samples.
#'   - **`frequency_dist`** (one row per scale value and count that occurs in
#'   any sample): `value`, `count`, `n_samples`, the number of samples in which
#'   the value occurs `count` times, and `n_samples_single_peak`, the same
#'   among the single-peaked samples.
#'
#'   Sample numbers above about 1.8e19 are capped there.
#'
#' @export
#'
#' @examples
#' # Some samples are single-peaked:
#' closure_count_details(
#'   mean = "3.5",
#'   sd = "1.2",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Samples exist, but none is single-peaked:
#' closure_count_details(
#'   mean = "4.00",
#'   sd = "1.73",
#'   n = 5,
#'   scale_min = 1,
#'   scale_max = 7
#' )$metrics_main

closure_count_details <- function(
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  rounding = "up_or_down",
  threshold = 5
) {
  out <- call_count_fn(
    rust_fn = count_closure_details,
    mean = mean,
    sd = sd,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    rounding = rounding,
    threshold = threshold
  )

  # Count ranges come back empty if there is no sample in the group
  k <- length(out$value)
  fill_na <- function(x, na) {
    if (length(x) == 0L) rep(na, k) else x
  }

  list(
    metrics_main = tibble::tibble(
      samples_all = out$samples_all,
      samples_single_peak = out$samples_single_peak
    ),
    modality_counts = tibble::tibble(
      value = out$value,
      count_lo = fill_na(out$count_lo, NA_integer_),
      count_hi = fill_na(out$count_hi, NA_integer_),
      count_mean = fill_na(out$count_mean, NA_real_),
      single_peak_count_lo = fill_na(out$single_peak_count_lo, NA_integer_),
      single_peak_count_hi = fill_na(out$single_peak_count_hi, NA_integer_),
      single_peak_count_mean = fill_na(out$single_peak_count_mean, NA_real_)
    ),
    frequency_dist = tibble::tibble(
      value = out$dist_value,
      count = out$dist_count,
      n_samples = out$dist_n_samples,
      n_samples_single_peak = out$dist_n_samples_single_peak
    )
  )
}


# Checks the inputs of the counting functions, unrounds `mean` and `sd`, and
# calls the Rust function `rust_fn` -- the same procedure as in
# `generate_from_mean_sd_n()`, and therefore `closure_generate()`.
call_count_fn <- function(
  rust_fn,
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  rounding,
  threshold
) {
  check_single(mean, "character")
  check_single(sd, "character")
  check_single(n, c("double", "integer"))
  check_single(scale_min, c("double", "integer"))
  check_single(scale_max, c("double", "integer"))
  check_single(rounding, "character")
  check_single(threshold, c("double", "integer"))

  check_whole_number(n)
  check_whole_number(scale_min)
  check_whole_number(scale_max)

  check_numeric_string(mean)
  check_numeric_string(sd)

  check_scale(scale_min, scale_max, as.numeric(mean), allow_one_point = FALSE)

  # See `generate_from_mean_sd_n()` for why the interval centers are used
  mean_sd_unrounded <- unround_center_error(
    x = c(mean, sd),
    rounding = rounding,
    threshold = threshold
  )

  # Call into the Rust implementation
  out <- rust_fn(
    mean = mean_sd_unrounded$center[1],
    sd = mean_sd_unrounded$center[2],
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    rounding_error_mean = mean_sd_unrounded$error[1],
    rounding_error_sd = mean_sd_unrounded$error[2]
  )

  # The Rust side signals failure by returning an error message as a string
  if (is.character(out)) {
    abort_in_export(
      "Counting CLOSURE samples failed on the Rust level.",
      "x" = "{out}"
    )
  }

  out
}


# (By Claude:) Each combination starts with two numbers i,j where scale_min <= i
# <= j <= scale_max
# This is equivalent to choosing 2 numbers with replacement where order doesn't
# matter The formula is: (n+1) * n / 2 where n is the range size

#' @rdname closure_count_all
#' @export
closure_count_initial <- function(scale_min, scale_max) {
  check_single(scale_min, c("double", "integer"))
  check_single(scale_max, c("double", "integer"))

  check_scale(scale_min, scale_max)

  range_size <- scale_max - scale_min + 1

  (range_size * (range_size + 1)) / 2
}
