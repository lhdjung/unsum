
<!-- README.md is generated from README.Rmd. Please edit that file -->

# unsum: reconstruct raw data from summary statistics

<!-- badges: start -->

<!-- badges: end -->

The goal of unsum is to **un**do **sum**marization: reconstruct all
possible samples that may underlie a given set of summary statistics. It
currently supports sets of mean, SD, sample size, and scale bounds. This
can be useful in forensic metascience to identify impossible or
implausible reported numbers.

The package features *CLOSURE: complete listing of original samples of
underlying raw evidence*, a fast algorithm implemented in Rust. Go to
[**Get started**](https://lhdjung.github.io/unsum/articles/unsum.html)
to learn how to use it.

CLOSURE is exhaustive, which makes it computationally intensive. If your
code takes too long to run, consider using
[SPRITE](https://lukaswallrich.github.io/rsprite2//) instead (see
*Previous work* below).

## Installation

You can install unsum with either of these:

``` r
install.packages("unsum")
# or
pak::pkg_install("unsum")
```

Your R version should be 4.2.0 or more recent.

## Demo

``` r
library(unsum)
```

Start with `closure_generate()`, the package’s main function. It creates
all possible samples:

``` r
data <- closure_generate(
  mean = "2.7",
  sd = "1.9",
  n = 130,
  scale_min = 1,
  scale_max = 5
)
#> → Just a second...
#> 
#> ✔ All CLOSURE results found

data
#> $inputs
#> # A tibble: 1 × 7
#>   mean  sd        n scale_min scale_max rounding   threshold
#>   <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 2.7   1.9     130         1         5 up_or_down         5
#> 
#> $metrics_main
#> # A tibble: 1 × 3
#>   samples_initial samples_all values_all
#>             <int>       <dbl>      <dbl>
#> 1              15        5359     696670
#> 
#> $metrics_horns
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.881     0.5 0.0237 0.0269 0.0171 0.849  0.876 0.943 0.0940
#> 
#> $frequency
#> # A tibble: 15 × 5
#>    samples   value f_average f_absolute f_relative
#>    <chr>     <int>     <dbl>      <dbl>      <dbl>
#>  1 all           1     67.0      358972     0.515 
#>  2 all           2      5.63      30170     0.0433
#>  3 all           3      4.09      21940     0.0315
#>  4 all           4      5.63      30162     0.0433
#>  5 all           5     47.7      255426     0.367 
#>  6 horns_min     1     63.1        2651     0.486 
#>  7 horns_min     2      8.10        340     0.0623
#>  8 horns_min     3      5           210     0.0385
#>  9 horns_min     4      8.24        346     0.0634
#> 10 horns_min     5     45.5        1913     0.350 
#> 11 horns_max     1     71.2         285     0.548 
#> 12 horns_max     2      2.25          9     0.0173
#> 13 horns_max     3      1.75          7     0.0135
#> 14 horns_max     4      1.75          7     0.0135
#> 15 horns_max     5     53           212     0.408 
#> 
#> $results
#> # A tibble: 5,359 × 3
#>       id sample      horns
#>    <int> <list>      <dbl>
#>  1     1 <int [130]> 0.851
#>  2     2 <int [130]> 0.851
#>  3     3 <int [130]> 0.855
#>  4     4 <int [130]> 0.856
#>  5     5 <int [130]> 0.849
#>  6     6 <int [130]> 0.851
#>  7     7 <int [130]> 0.851
#>  8     8 <int [130]> 0.855
#>  9     9 <int [130]> 0.859
#> 10    10 <int [130]> 0.852
#> # ℹ 5,349 more rows
```

Visualize the overall distribution of values found in the samples:

``` r
closure_plot_bar(data)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="Barplot of `data`, the CLOSURE output. It specifically visualizes the `f_average` column of the `frequency` tibble, but also gives percentage figures, similar to the `f_relative` column. The overall shape is a somewhat polarized distribution." width="100%" />

## Previous work

[SPRITE](https://lukaswallrich.github.io/rsprite2/) generates random
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
[closure-core](https://github.com/lhdjung/closure-core), via the amazing
[extendr](https://extendr.github.io/) framework. Rust code tends to run
much faster than R or Python code, which is required for many
applications of CLOSURE.
