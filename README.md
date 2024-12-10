
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CLOSURE: Complete Listing of Original Samples of Underlying Raw Evidence

<!-- badges: start -->
<!-- badges: end -->

The closure package enables efficient reconstruction of all possible
datasets that may underlie a given set of summary statistics: mean, SD,
sample size, and scale bounds. This can be useful in error detection to
identify impossible or implausible reported numbers.

## TODO: add section with most important use cases here

(…)

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

## Previous work

[SPRITE](https://lukaswallrich.github.io/rsprite2) generates random
datasets that could have led to the reported statistics. However,
CLOSURE is exhaustive, so it always finds all such datasets.

[GRIM and GRIMMER](https://lhdjung.github.io/scrutiny/) test reported
summary statistics for consistency, but CLOSURE is the ultimate
consistency test: if it finds at least one distribution, the statistics
are consistent; and if not, they cannot all be correct.

To the best of our knowledge,
[CORVIDS](https://github.com/katherinemwood/corvids) was the first
technique to reconstruct all possible underlying datasets. However, it
takes very long to run, often prohibitively so. This is partly because
the code is written in Python, but the algorithm is also inherently much
more complex than CLOSURE.

## About the package

The R package closure provides easy access to the algorithm’s
implementation in Rust,
[closure-core](https://crates.io/crates/closure-core), via the amazing
[extendr](https://extendr.github.io/) framework. Rust code tends to run
much faster than R or Python code, and since CLOSURE is very intensive
computationally, high performance is required for many practical
applications.
