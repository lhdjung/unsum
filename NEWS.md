# unsum (development version)

## New features

-   Added writing mode in `closure_generate()` via the new `path` argument.

-   Modified `closure_generate()`'s output to incorporate the horns index values for each generated sample, summary statistics about them, and frequencies based on the minimal and maximal horns values.

-   Added `closure_plot_bar_min_max()` to compare the subsets of samples with minimal and maximal variability, as measured by `horns()`.

## Breaking changes

-   Removed `closure_horns_analyze()`. Its functionality was integrated into `closure_generate()` for simplicity and ease of use.
-   Removed the `rounding_error_mean` and `rounding_error_sd` arguments from `closure_generate()`. They are not needed for users. If anything, you can use the `rounding` argument instead.
-   Redesigned `closure_read()` to control which parts are read in via the new `include` and `samples_cap` arguments.

## Bugfixes

-   Fixed a bug that caused `closure_plot_ecdf()` to return clearly wrong results if the scale did not start at 1.

# unsum 0.2.0

-   Initial CRAN submission.
-   Added `closure_horns_analyze()` and `closure_horns_histogram()`.
-   Removed vignette on installing Rust since users will not need it when the package is on CRAN.
-   Fixed examples that causes CRAN check issues.
