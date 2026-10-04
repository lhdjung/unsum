# Count CLOSURE samples in advance

Determine how many samples
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)
would find for a given set of summary statistics.

- `closure_count_all()` counts all CLOSURE samples that correspond to
  the input summary statistics, but without actually generating the
  samples. This is much faster than
  [`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)
  if there are many results.

- `closure_count_initial()` only counts the first round of samples, from
  which all other ones would be generated. Based on scale range only.

This can help predict how much time
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)
would take, and avoid prohibitively long runs.

## Usage

``` r
closure_count_all(
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  rounding = "up_or_down",
  threshold = 5
)

closure_count_initial(scale_min, scale_max)
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

Integer (length 1).

## Examples

``` r
closure_count_all(
  mean = "3.5",
  sd = "1.7",
  n = 70,
  scale_min = 1,
  scale_max = 5
)
#> [1] 2492

closure_count_initial(scale_min = 1, scale_max = 5)
#> [1] 15
```
