# unsum (development version)

## New features

-   Modified `closure_generate()`'s output to incorporate the horns index values for each generated sample, summary statistics about them, and frequencies based on the minimal and maximal horns values.

-   Added writing mode in `closure_generate()` via the new `path` argument. This allows you to save large data to disk immediately, preventing a risk of data loss.

-   Added `closure_plot_bar_min_max()` to compare the subsets of samples with minimal and maximal variability, as measured by `horns()`.

## Breaking changes

-   Reworked `closure_plot_ecdf()`:
    -   It now shows 3 lines by default — overall mean, min horns index, and max horns index `(samples = "mean_min_max")`. The old default was `samples = "mean"` for a single line.

    -   Accordingly, the `line_color` argument was replaced by `line_color_single` and `line_color_multiple`.

    -   The `pad` argument now defaults to `FALSE`.
-   Renamed `closure_horns_histogram()` to `closure_plot_horns_histogram()`. This way, all CLOSURE plot functions start on `closure_plot_`.
-   Redesigned `closure_read()` to control which parts are read in via the new `include` and `samples_cap` arguments.
-   Removed `closure_horns_analyze()`. Its functionality was integrated into `closure_generate()` for simplicity and ease of use.
-   Removed the `rounding_error_mean` and `rounding_error_sd` arguments from `closure_generate()`. They are not needed for users. If anything, you can use the `rounding` argument instead.

## Bugfixes

-   Fixed a bug that caused `closure_plot_ecdf()` to return clearly wrong results if the scale did not start at 1.

# unsum 0.2.0

-   Initial CRAN submission.
-   Added `closure_horns_analyze()` and `closure_horns_histogram()`.
-   Removed vignette on installing Rust since users will not need it when the package is on CRAN.
-   Fixed examples that causes CRAN check issues.
