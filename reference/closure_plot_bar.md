# Visualize CLOSURE data in a barplot

Call `closure_plot_bar()` to get a barplot of CLOSURE results.

For each scale value, the bars show how often this value appears in the
mean samples with the minimum or maximum horns index (\\h\\). This
displays the typical sample with the least or most amount of variance
from among all CLOSURE samples.

As CLOSURE finds all possible samples, this plot shows the min and max
*possible* amount of variance given the input statistics.

## Usage

``` r
closure_plot_bar(
  data,
  min_max = c("both", "min", "max"),
  format = c("percent", "absolute_percent", "absolute", "relative"),
  samples = c("mean", "all"),
  overlay = c("all_avg", "none", "interval", "pointinterval", "dots"),
  facet_labels = c("Minimal variance", "Maximal variance"),
  facet_labels_parens = "h",
  bar_alpha = 0.75,
  bar_color = "#5D3FD3",
  show_text = TRUE,
  text_color = bar_color,
  text_size = 12,
  text_offset = 0.05,
  mark_thousand = ",",
  mark_decimal = "."
)
```

## Arguments

- data:

  List returned by
  [`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)
  or
  [`closure_read()`](https://lhdjung.github.io/unsum/reference/closure_write.md).

- min_max:

  String (length 1). Which plot panel(s) to show? Options are `"both"`
  (the default), `"min"`, and `"max"`.

- format:

  String (length 1). What should the bars show? The default is
  `"percent"`. Similarly, `"absolute_percent"` shows the count of each
  scale value and its percentage of all values. Other options are
  `"absolute"` and `"relative"` frequencies.

- samples:

  String (length 1). How to aggregate the samples? Either take the
  average sample (`"mean"`, the default) or the sum of all samples
  (`"all"`). This only matters if absolute frequencies are shown.

- overlay:

  String (length 1). Visualization mode for the frequency distribution
  across all samples, overlaid behind the main bars. Only applies when
  `samples = "mean"`. Options:

  - `"all_avg"` (default): gray background bars showing the mean
    frequency across all samples.

  - `"none"`: no overlay.

  - `"interval"`: nested quantile intervals via
    [`ggdist::stat_interval()`](https://mjskay.github.io/ggdist/reference/stat_interval.html)
    (requires the `ggdist` package and `include = "all"` in
    [`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)).

  - `"pointinterval"`: median point with quantile interval lines via
    [`ggdist::stat_pointinterval()`](https://mjskay.github.io/ggdist/reference/stat_pointinterval.html)
    (same requirements as `"interval"`).

  - `"dots"`: quantile dot plot via
    [`ggdist::stat_dots()`](https://mjskay.github.io/ggdist/reference/stat_dots.html)
    (same requirements as `"interval"`).

- facet_labels:

  String (length 2). Labels of the two individual panels. Set it to
  `NULL` to remove the labels. Default is
  `c("Minimal variance", "Maximal variance")`.

- facet_labels_parens:

  String (length 1). Italicized part of the facet labels inside the
  parentheses. Set it to `NULL` to remove the parentheses altogether.
  See details. Default is `"h"`.

- bar_alpha:

  Numeric (length 1). Opacity of the bars. Default is `0.75`.

- bar_color:

  String (length 1). Color of the bars. Default is `"#5D3FD3"`, a purple
  color.

- show_text:

  Logical (length 1). Should the bars be labeled with the corresponding
  frequencies? Default is `TRUE`.

- text_color:

  String (length 1). Color of the frequency labels. By default, the same
  as `bar_color`.

- text_size:

  Numeric (length 1). Base font size in pt. Default is `12`.

- text_offset:

  Numeric (length 1). Distance between the text labels and the bars.
  Default is `0.05`.

- mark_thousand, mark_decimal:

  Strings (length 1 each). Delimiters between groups of digits in text
  labels. Defaults are `","` for `mark_thousand` (e.g., `"20,000"`) and
  `"."` for `mark_decimal` (e.g., `"0.15"`).

## Value

A ggplot object.

## Details

By default, both faceted plots have a label that includes their horns
index (\\h\\); see
[`horns()`](https://lhdjung.github.io/unsum/reference/horns.md). You can
remove the parenthesized part using `facet_labels_parens = NULL` or the
entire label using `facet_labels = NULL`.

## See also

[`closure_plot_ecdf()`](https://lhdjung.github.io/unsum/reference/closure_plot_ecdf.md),
an alternative visualization.

## Examples

``` r
# Create CLOSURE data first:
data <- closure_generate(
  mean = "2.9",
  sd = "0.5",
  n = 70,
  scale_min = 1,
  scale_max = 5
)
#> 
#> ✔ All CLOSURE results found

# Even with minimal and maximal variance,
# the results are almost the same:
closure_plot_bar(data)


# Only show the minimum-variance panel:
closure_plot_bar(data, min_max = "min")
```
