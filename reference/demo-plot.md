# Visualize example distributions and their \\h\\ values

The `demo_plot_*()` functions are variants of `closure_plot_*()` that
directly visualize a given frequency distribution. Their purpose is to
illustrate general points about CLOSURE-type techniques and the horns
index.

- `demo_plot_bar()` is like
  [`closure_plot_bar()`](https://lhdjung.github.io/unsum/reference/closure_plot_bar.md)
  except it is never faceted.

- `demo_plot_horns_histogram()` is like
  [`closure_plot_horns_histogram()`](https://lhdjung.github.io/unsum/reference/horns-frequency.md).

- `demo_plot_ecdf()` is like
  [`closure_plot_ecdf()`](https://lhdjung.github.io/unsum/reference/closure_plot_ecdf.md).

The top line shows the horns index of the given distribution (\\h\\) and
the horns index of a hypothetical uniform distribution with the same
number of scale points (\\h_u\\). See
[`horns()`](https://lhdjung.github.io/unsum/reference/horns.md) and
[`horns_uniform()`](https://lhdjung.github.io/unsum/reference/horns.md)
for the corresponding functions.

## Usage

``` r
demo_plot_bar(
  freqs,
  format = c("percent", "absolute_percent", "absolute", "relative"),
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

- freqs:

  Numeric. Vector of relative or absolute frequencies to visualize.

- format:

  String (length 1). What should the bars show? The default is
  `"percent"`. Similarly, `"absolute_percent"` shows the count of each
  scale value and its percentage of all values. Other options are
  `"absolute"` and `"relative"` frequencies.

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

In keeping with the forensic metascience tradition of tortured
backronyms, DEMO stands for "displaying examples of meticulous
operation".

## Examples

``` r
# Zero variance: h = 0
demo_plot_bar(freqs = c(0, 40, 0, 0, 0))


# Perfect "horns of no confidence": h = 1
demo_plot_bar(freqs = c(20, 0, 0, 0, 20))


# Grouped around h = ~0.44, the uniform horns index
# for 7-point scales
demo_plot_bar(freqs = 21:27)
```
