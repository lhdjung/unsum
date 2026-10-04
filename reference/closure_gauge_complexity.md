# Heuristic to predict CLOSURE runtime

Before you run
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md),
you may want to get a sense of the time it will take to run. Use
`closure_gauge_complexity()` to compute a heuristics-based complexity
score. For reference, here is how it determines the messages in
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md):

|              |                               |
|--------------|-------------------------------|
| **Value**    | **Message**                   |
| if \< 1      | (no message)                  |
| else if \< 2 | "Just a second..."            |
| else if \< 3 | "This could take a minute..." |
| else         | "NOTE: Long runtime ahead!"   |

## Usage

``` r
closure_gauge_complexity(mean, sd, n, scale_min, scale_max)
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

## Value

Numeric (length 1).

## Details

The result of this function is hard to interpret. All it can do is to
convey an idea about the likely runtime of CLOSURE. This is because the
input parameters interact in highly dynamic ways, which makes prediction
difficult.

In addition, even progress bars or updates at regular intervals (e.g.,
"10% complete") prove to be extremely challenging: the Rust code
computes CLOSURE results in parallel, which makes it hard to get an
overview of the total progress across all cores; and especially to
display such information on the R level.

## Examples

``` r
# Low SD, N, and scale range:
closure_gauge_complexity(
  mean = 2.55,
  sd = 0.85,
  n = 84,
  scale_min = 1,
  scale_max = 5
)
#> [1] 0.3521825

# Somewhat higher:
closure_gauge_complexity(
  mean = 4.26,
  sd = 1.58,
  n = 100,
  scale_min = 1,
  scale_max = 7
)
#> [1] 2.477121

# Very high:
closure_gauge_complexity(
  mean = 3.81,
  sd = 3.09,
  n = 156,
  scale_min = 1,
  scale_max = 7
)
#> [1] 3.39794
```
