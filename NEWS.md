# unsum (development version)

This is the first major version of unsum. As such, it introduces many new features, but also many breaking changes. You may need to adjust your code if you have used unsum before.

In the big picture, this version unifies and consolidates the system of CLOSURE functions. It moves the horns index calculation down to the Rust level, where it is now conducted alongside CLOSURE itself. Their results are returned together. This reaffirms the central status of `closure_generate()`: every other function that uses CLOSURE results can now immediately follow up on `closure_generate()`, without any intermediaries.

Another focus of this release is visualization, with new plotting functions and improvements on existing ones.

## New features

-   Modified `closure_generate()`'s output to incorporate the horns index values for each generated sample, summary statistics about them, and frequencies based on the minimal and maximal horns values.

-   Added writing mode in `closure_generate()` via the new `path` argument. This allows you to save large data to disk immediately, preventing a risk of data loss.

-   Added a `technique` column at the start of `closure_generate()`'s output. This is for clarity: it says `"CLOSURE"` here, and the output of any technique to be implemented in the future will be disambiguated in the same way.

-   Added `closure_plot_horns_histogram()` to visualize the distribution of horns values as a whole.

## Breaking changes

-   Reworked `closure_plot_ecdf()`:
    -   It now shows 3 lines by default — overall mean, min horns index, and max horns index `(samples = "mean_min_max")`, with a legend that includes the horns index values of each category. The old default was `samples = "mean"` for a single line and no legend.

    -   Accordingly, the `line_color` argument was replaced by `line_color_single` and `line_color_multiple`.

    -   The `pad` argument is now a string with three alternatives.

    -   Added `legend_title` and `mark_decimal` arguments.
-   Remodeled `closure_plot_bar()` to show two panels instead of one. It now compares the subsets of samples with minimal and maximal variability, as measured by `horns()`.
-   Redesigned `closure_read()` to control which parts are read in via the new `include` and `samples_cap` arguments.
-   Renamed the `frequency` argument of `closure_plot_bar()` to `format`, for clarity. There are many mentions of "frequency" in unsum, so it is good to disambiguate. In particular, the new `demo_plot_bar()` has `freqs` as its first argument.
-   Renamed the `"absolute-percent"` option of `format` (the former `frequency`) in `closure_plot_bar()` to `"absolute_percent"`, for consistency with other multi-word strings in the package.
-   Removed `closure_horns_analyze()`. Its functionality was integrated into `closure_generate()` for simplicity and ease of use.
-   Removed `closure_horns_histogram()` because its functionality has now been replaced by `closure_plot_horns_histogram()`.
-   Removed the `rounding_error_mean` and `rounding_error_sd` arguments from `closure_generate()`. They are not needed for users. If anything, you can use the `rounding` argument instead.

## Bugfixes

-   Fixed a bug that caused `closure_plot_ecdf()` to return clearly wrong results if the scale did not start at 1.
-   Fixed bugs in `closure_plot_bar()` that could cause imprecision in the ways that percentages were rounded for display using `frequency = "percent"` or `frequency = "absolute-percent"` (see above for new syntax). This was only intended to limit the length of the percentage text labels, but it could affect the bar sizes, as well.

## Lifecycle updates

-   The package now requires ggplot2 version 3.4.0 or later and ggtext.

# unsum 0.2.0

-   Initial CRAN submission.
-   Added `closure_horns_analyze()` and `closure_horns_histogram()`.
-   Removed vignette on installing Rust since users will not need it when the package is on CRAN.
-   Fixed examples that causes CRAN check issues.
