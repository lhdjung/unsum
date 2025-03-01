
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CLOSURE: Complete Listing of Original Samples of Underlying Raw Evidence

<!-- badges: start -->
<!-- badges: end -->

The unsum package enables efficient reconstruction of all possible
datasets that may underlie a given set of summary statistics: mean, SD,
sample size, and scale bounds. This can be useful in error detection to
identify impossible or implausible reported numbers.

CLOSURE is exhaustive, which makes it computationally intensive. If your
code takes too long to run, consider using
[SPRITE](https://lukaswallrich.github.io/rsprite2/) instead (see
*Previous work* below).

## Installation

You can install the development version of unsum from
[GitHub](https://github.com/) with either of these:

``` r
remotes::install_github("lhdjung/unsum")
# or
pak::pak("lhdjung/unsum")
```

Your R version should be 4.2.0 or more recent. To run unsum, you also
need a Rust installation; see `vignette("install-rust")`.

## Get started

Start with `closure_combine()`, the package’s main function. It creates
all possible samples:

``` r
library(unsum)

data <- closure_combine(
  mean = "2.7",
  sd = "1.9",
  n = 100,
  scale_min = 1,
  scale_max = 5
)

data
#> $metadata
#> # A tibble: 1 × 8
#>   mean  sd        n scale_min scale_max combos_initial combos_all values_all
#>   <chr> <chr> <dbl>     <dbl>     <dbl>          <int>      <int>      <int>
#> 1 2.7   1.9     100         1         5             15       2256     225600
#> 
#> $frequency
#> # A tibble: 5 × 3
#>   value f_absolute f_relative
#>   <int>      <int>      <dbl>
#> 1     1     116060     0.514 
#> 2     2       9908     0.0439
#> 3     3       7160     0.0317
#> 4     4       9912     0.0439
#> 5     5      82560     0.366 
#> 
#> $results
#> # A tibble: 2,256 × 2
#>       id combination
#>    <int> <list>     
#>  1     1 <int [100]>
#>  2     2 <int [100]>
#>  3     3 <int [100]>
#>  4     4 <int [100]>
#>  5     5 <int [100]>
#>  6     6 <int [100]>
#>  7     7 <int [100]>
#>  8     8 <int [100]>
#>  9     9 <int [100]>
#> 10    10 <int [100]>
#> # ℹ 2,246 more rows
```

Visualize the overall distribution of values found in the combinations:

``` r
closure_plot_bar(data)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## Previous work

[SPRITE](https://lukaswallrich.github.io/rsprite2) generates random
datasets that could have led to the reported statistics. CLOSURE is
exhaustive, so it always finds all possible datasets, not just a random
sample of them. For the same reason, SPRITE runs fast when CLOSURE may
take too long.

[GRIM and GRIMMER](https://lhdjung.github.io/scrutiny/) test reported
summary statistics for consistency, but CLOSURE is the ultimate
consistency test: if it finds at least one distribution, the statistics
are consistent; and if not, they cannot all be correct.

[CORVIDS](https://github.com/katherinemwood/corvids) deserves credit as
the first technique to reconstruct all possible underlying datasets.
However, it takes very long to run, often prohibitively so. This is
partly because the code is written in Python, but the algorithm is also
inherently much more complex than CLOSURE.

## About

The CLOSURE algorithm was originally written [in
Python](https://github.com/larigaldie-n/CLOSURE-Python) by Nathanael
Larigaldie. The R package unsum provides easy access to an optimized
implementation in Rust,
[closure-core](https://crates.io/crates/closure-core), via the amazing
[extendr](https://extendr.github.io/) framework. Rust code tends to run
much faster than R or Python code, which is required for many
applications of CLOSURE.
