# unsum 0.3.0

This could easily be the first major version of unsum because it contains many new features, but also many breaking changes – you may need to adjust your code if you have used unsum before. However, it does not yet rise to the level where I can promise a stable API, so it is officially a minor release.

I do not think this is an ideal versioning model, and future releases will not follow it. The current release became necessary due to a combination of breaking changes in upstream dependencies and increasingly strict CRAN policies. Thanks to all maintainers of this crucial infrastructure.

## Caveat
I have become more cautious about the inferences allowed by CLOSURE (and SPRITE) than I was at the time of the 0.2.0 release. Users are advised to exercise caution when using these methods and drawing conclusions based on them. Forensic metascience methods generally require validation, and there is still much work to be done on both of these methods. There are plans to conduct this work in the future.

## Big picture
This version unifies and consolidates the system of CLOSURE functions and remodels its output to include much new information. It also moves the horns index calculation down to the Rust level, where it is now conducted alongside CLOSURE itself. Their results are returned together. This reaffirms the central status of `closure_generate()`: every other function that uses CLOSURE results can now immediately follow up on `closure_generate()`, without any intermediaries.

The other big change is SPRITE support. SPRITE is also implemented in Rust and generally mirrors CLOSURE in its output and API.

Another focus of this release is visualization, with new plotting functions and improvements on existing ones.

## New features

-   SPRITE is now implemented in unsum. It is centered around `sprite_generate()` and features downstream visualization functions, such as `sprite_plot_bar()`.

-   `closure_count_all()` returns the total number of samples CLOSURE will find without running CLOSURE itself. Useful to assess the complexity of future runs, but not yet used to its full potential.

-   `demo_plot_bar()` can be used to showcase principles of reconstruction methods and the horns index by using arbitrary example distributions.

-   Modified `closure_generate()`'s output to incorporate the horns index values for each generated sample, summary statistics about them, and frequencies based on the minimal and maximal horns index values.

-   Added writing mode in `closure_generate()` via the new `path` argument. This allows you to save large data to disk immediately, preventing a risk of data loss.

-   Added a `technique` column at the start of `closure_generate()`'s output. This is for clarity: it says `"CLOSURE"` here, and the output of any technique to be implemented in the future will be disambiguated in the same way.

-   Added `closure_plot_horns_histogram()` to visualize the distribution of horns index values as a whole.

## Breaking changes

-   CLOSURE (and SPRITE) output is now an S7 object that is essentially a list of tibbles. You can access it like other tibbles, but manipulation is intentionally restricted to preserve authenticity of results.

-   Reworked `closure_plot_ecdf()`:
    -   It now shows 3 lines by default — overall mean, min horns index, and max horns index `(samples = "mean_min_max")`, with a legend that includes the horns index values of each category. The old default was `samples = "mean"` for a single line and no legend.

    -   Accordingly, the `line_color` argument was replaced by `line_color_single` and `line_color_multiple`.

    -   The `pad` argument is now a string with three alternatives.

    -   Added `legend_title` and `mark_decimal` arguments.
-   Remodeled `closure_plot_bar()` to show two panels instead of one. It now compares the subsets of samples with minimal and maximal variability, as measured by `horns()`. Its `format` argument now defaults to `"percent"` rather than `"absolute-percent"`, for readability. It also gained new arguments to control the new two-panel layout: `min_max`, `overlay`, `facet_labels`, and `facet_labels_parens`.
-   Redesigned `closure_read()` to control which parts are read in via the new `include` and `samples_cap` arguments.
-   Renamed the `frequency` argument of `closure_plot_bar()` to `format`, for clarity. There are many mentions of "frequency" in unsum, so it is good to disambiguate. In particular, the new `demo_plot_bar()` has `freqs` as its first argument.
-   Renamed the `"absolute-percent"` option of `format` (the former `frequency`) in `closure_plot_bar()` to `"absolute_percent"`, for consistency with other multi-word strings in the package.
-   Removed `closure_horns_analyze()`. Its functionality was integrated into `closure_generate()` for simplicity and ease of use.
-   Removed `closure_horns_histogram()` because its functionality has now been replaced by `closure_plot_horns_histogram()`.
-   Removed the `rounding_error_mean` and `rounding_error_sd` arguments from `closure_generate()`. They are not needed for users. If anything, you can use the `rounding` argument instead.
-   Also removed the `warn_if_empty` argument from `closure_generate()`. It did not fulfill much of a purpose.

## Bugfixes

-   Fixed a bug that caused `closure_plot_ecdf()` to return clearly wrong results if the scale did not start at 1.
-   Fixed bugs in `closure_plot_bar()` that could cause imprecision in the ways that percentages were rounded for display using `frequency = "percent"` or `frequency = "absolute-percent"` (see above for new syntax). This was only intended to limit the length of the percentage text labels, but it could affect the bar sizes, as well.
-   Fixed a mismatch between `closure_write()` and `closure_read()`: the two disagreed about which files make up a results folder, so writing results and reading them back in again failed.

## Lifecycle updates

-   The package now requires ggplot2 version 3.4.0 or later and ggtext.
-   The package no longer depends on readr.
-   New dependencies: S7, ggtext, grid, and stats. (The last two are standard packages.)
-   Updated the Rust dependency on extendr to version 0.9.0, which no longer calls the non-API entry point `R_NamespaceRegistry`.

# unsum 0.2.0

-   Initial CRAN submission.
-   Added `closure_horns_analyze()` and `closure_horns_histogram()`.
-   Removed vignette on installing Rust since users will not need it when the package is on CRAN.
-   Fixed examples that causes CRAN check issues.
