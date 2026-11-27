# Count CLOSURE samples per scale value, and single-peaked ones

`closure_count_details()` goes beyond
[`closure_count_all()`](https://lhdjung.github.io/unsum/reference/closure_count_all.md):
for each scale value and each count, it says in how many CLOSURE samples
the value occurs that many times. It does this over all samples and over
the single-peaked ones, without generating any sample.

A sample is single-peaked if its counts rise to a maximum and then fall,
with ties allowed but no dip at all. This needs no cutoff for what
counts as a peak, unlike the shape classes of
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md).

The function settles whether any single-peaked sample has the reported
statistics, and what the counts of every such sample would have to be.
It also gives the range of each scale value's count across all samples,
like `modality_counts` in the output of
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md).
All of this is exact, and it remains feasible at sample sizes where
generating the samples is not. It takes about one and a half times as
long as
[`closure_count_all()`](https://lhdjung.github.io/unsum/reference/closure_count_all.md).

Like
[`closure_count_all()`](https://lhdjung.github.io/unsum/reference/closure_count_all.md),
it assumes a single item per respondent.

## Usage

``` r
closure_count_details(
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  rounding = "up_or_down",
  threshold = 5
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

  Integers (length 1 each). Minimum and maximum of the scales to which
  the reported statistics refer.

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

## Value

Named list of three tibbles:

- **`metrics_main`** (one row): `samples_all`, the number of all
  samples, as returned by
  [`closure_count_all()`](https://lhdjung.github.io/unsum/reference/closure_count_all.md),
  and `samples_single_peak`, the number of single-peaked samples. Both
  are doubles.

- **`modality_counts`** (one row per scale value): `value`, then
  `count_lo`, `count_hi`, and `count_mean`, the smallest, largest, and
  average count of the value across all samples; then the same over the
  single-peaked samples (`single_peak_count_lo` etc.). They are `NA` if
  there are no such samples.

- **`frequency_dist`** (one row per scale value and count that occurs in
  any sample): `value`, `count`, `n_samples`, the number of samples in
  which the value occurs `count` times, and `n_samples_single_peak`, the
  same among the single-peaked samples.

Sample numbers above about 1.8e19 are capped there.

## Examples

``` r
# Some samples are single-peaked:
closure_count_details(
  mean = "3.5",
  sd = "1.2",
  n = 70,
  scale_min = 1,
  scale_max = 5
)
#> $metrics_main
#> # A tibble: 1 × 2
#>   samples_all samples_single_peak
#>         <dbl>               <dbl>
#> 1        7912                 892
#> 
#> $modality_counts
#> # A tibble: 5 × 7
#>   value count_lo count_hi count_mean single_peak_count_lo single_peak_count_hi
#>   <dbl>    <int>    <int>      <dbl>                <int>                <int>
#> 1     1        0       13       5.06                    1                    8
#> 2     2        0       27       9.94                    5                   17
#> 3     3        0       43      15.9                     7                   27
#> 4     4        0       57      23.2                    16                   40
#> 5     5        0       27      15.9                     8                   21
#> # ℹ 1 more variable: single_peak_count_mean <dbl>
#> 
#> $frequency_dist
#> # A tibble: 172 × 4
#>    value count n_samples n_samples_single_peak
#>    <dbl> <int>     <dbl>                 <dbl>
#>  1     1     0       630                     0
#>  2     1     1       672                     7
#>  3     1     2       714                    35
#>  4     1     3       756                    89
#>  5     1     4       798                   152
#>  6     1     5       838                   224
#>  7     1     6       833                   241
#>  8     1     7       754                   134
#>  9     1     8       630                    10
#> 10     1     9       504                     0
#> # ℹ 162 more rows
#> 

# Samples exist, but none is single-peaked:
closure_count_details(
  mean = "4.00",
  sd = "1.73",
  n = 5,
  scale_min = 1,
  scale_max = 7
)$metrics_main
#> # A tibble: 1 × 2
#>   samples_all samples_single_peak
#>         <dbl>               <dbl>
#> 1           2                   0
```
