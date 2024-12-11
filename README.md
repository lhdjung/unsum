
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CLOSURE: Complete Listing of Original Samples of Underlying Raw Evidence

<!-- badges: start -->
<!-- badges: end -->

The closure package enables efficient reconstruction of all possible
datasets that may underlie a given set of summary statistics: mean, SD,
sample size, and scale bounds. This can be useful in error detection to
identify impossible or implausible reported numbers.

CLOSURE is computationally intensive. If your code takes too long to
run, consider using [SPRITE](https://lukaswallrich.github.io/rsprite2/)
instead (see *Previous work* below).

## Installation

You can install the development version of closure from
[GitHub](https://github.com/) with either of these:

``` r
remotes::install_github("lhdjung/closure")
# or
pak::pak("lhdjung/closure")
```

Your R version should be 4.2.0 or more recent. To run closure, you also
need a Rust installation; see `vignette("install-rust")`.

## Get started

Start with `closure_combine()`, the package’s main function. It creates
all possible samples:

``` r
library(closure)

data <- closure_combine(
  mean = "2.7",
  sd = "1.9",
  n = 100,
  scale_min = 1,
  scale_max = 5
)

# Columns run from 1 to `n`, rows are combinations
data
#> $results
#> # A tibble: 216 × 100
#>       n1    n2    n3    n4    n5    n6    n7    n8    n9   n10   n11   n12   n13
#>    <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
#>  1     1     1     1     1     1     1     1     1     1     1     1     1     1
#>  2     1     1     1     1     1     1     1     1     1     1     1     1     1
#>  3     1     1     1     1     1     1     1     1     1     1     1     1     1
#>  4     1     1     1     1     1     1     1     1     1     1     1     1     1
#>  5     1     1     1     1     1     1     1     1     1     1     1     1     1
#>  6     1     1     1     1     1     1     1     1     1     1     1     1     1
#>  7     1     1     1     1     1     1     1     1     1     1     1     1     1
#>  8     1     1     1     1     1     1     1     1     1     1     1     1     1
#>  9     1     1     1     1     1     1     1     1     1     1     1     1     1
#> 10     1     1     1     1     1     1     1     1     1     1     1     1     1
#> # ℹ 206 more rows
#> # ℹ 87 more variables: n14 <int>, n15 <int>, n16 <int>, n17 <int>, n18 <int>,
#> #   n19 <int>, n20 <int>, n21 <int>, n22 <int>, n23 <int>, n24 <int>,
#> #   n25 <int>, n26 <int>, n27 <int>, n28 <int>, n29 <int>, n30 <int>,
#> #   n31 <int>, n32 <int>, n33 <int>, n34 <int>, n35 <int>, n36 <int>,
#> #   n37 <int>, n38 <int>, n39 <int>, n40 <int>, n41 <int>, n42 <int>,
#> #   n43 <int>, n44 <int>, n45 <int>, n46 <int>, n47 <int>, n48 <int>, …
#> 
#> $mean
#> [1] "2.7"
#> 
#> $sd
#> [1] "1.9"
#> 
#> $n
#> [1] 100
#> 
#> $scale_min
#> [1] 1
#> 
#> $scale_max
#> [1] 5
```

Visualize the overall distribution of values found in the combinations:

``` r
closure_plot_bar(data)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Get the same information in a data frame:

``` r
closure_summarize(data)
#> # A tibble: 5 × 3
#>   value f_absolute f_relative
#>   <int>      <int>      <dbl>
#> 1     1      11111     0.514 
#> 2     2        957     0.0443
#> 3     3        704     0.0326
#> 4     4        957     0.0443
#> 5     5       7871     0.364
```

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
Larigaldie. The R package closure provides easy access to an optimized
implementation in Rust,
[closure-core](https://crates.io/crates/closure-core), via the amazing
[extendr](https://extendr.github.io/) framework. Rust code tends to run
much faster than R or Python code, which is required for many
applications of CLOSURE.
