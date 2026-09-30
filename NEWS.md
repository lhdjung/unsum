# unsum (development version)

## Breaking changes

-   The `results` tibble no longer has a `sample` list-column. Instead, it has one integer column per scale value that counts how often this value occurs in each sample: `v1` for the 1s, `v2` for the 2s, etc. (`vn2` for -2, and `v1_5` for 1.5 with multi-item SPRITE). Because samples are sorted, these counts describe them completely, and they take up far less memory. To get a sample back as a vector, use `rep()` on the scale values and a row of counts.
-   In `frequency`, the `f_count` column is replaced by `f_expected`, the average count of each scale value within a group of samples, and `f_representative`, the counts in the group's medoid sample. The latter is `NaN` if results were written to disk via `path`. `f_relative` is now `f_expected / n` in memory and on disk alike.
-   The `value` columns of `frequency`, `frequency_dist`, `modality_counts`, and `modality_pairs` are now doubles because multi-item SPRITE scales have values like 1.5. These tables now have one row per such value.
-   `modality_conclusion` can now be `NA`: no sample with that shape was found, but the search was partial (SPRITE, or CLOSURE with `stop_after`). Its columns now describe the shapes of individual samples rather than of per-value count ranges.
-   Results written to disk use a new layout. Instead of sample.parquet and horns.parquet, a folder has counts.parquet, scale_values.parquet, and format.parquet, as well as the new modality files. Folders written by earlier versions of unsum can't be read anymore.

## New features

-   The output of `closure_generate()` and `sprite_generate()` gains three tibbles: `modality_shapes`, `modality_summary`, and `modality_prominence`. They describe how many samples take which shape (e.g., one or two modes). All modality tibbles are now also written to disk and read back from it.
-   Large result sets are much faster to return to R, and they take up much less memory.

## Changes in results

-   The underlying Rust crate, closure-core, was updated with many fixes. CLOSURE now finds samples that lie exactly on the bounds of the mean or SD (so there can be more samples than before), and `stop_after` is now an exact upper bound. SPRITE takes rounding errors literally, represents multi-item scales exactly, and reaches many more distinct samples than before.

## Bugfixes

-   Asymmetric rounding methods now search the correct interval. Previously, `closure_generate()`, `sprite_generate()`, and `closure_count_all()` passed a symmetric rounding error around the reported value to the Rust level even if `rounding` was, e.g., `"floor"` or `"ceiling"`. With `"floor"`, this meant only exact matches were found (usually none); with `"ceiling"`, too many.
-   `closure_generate(items = 2)` now throws an error instead of silently running CLOSURE with `items = 1`. CLOSURE only supports single-item scales; use `sprite_generate()` for others.
-   Results with negative scale values can now be read back from disk. The minus sign used to be confused with the dashes separating the values in the folder name.
-   Empty results written to disk via `path` can now be read back with `closure_read()` (and `sprite_read()`) instead of throwing an error.
-   Errors on the Rust level are now reported as R errors instead of a confusing `$ operator is invalid for atomic vectors` message. In writing mode, the partially created folder is removed.
-   `n`, `scale_min`, `scale_max`, `stop_after`, and `items` are now checked for being whole numbers, with a clear error message.
-   Printing in-memory results no longer shows an empty `$directory` element.

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
