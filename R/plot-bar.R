# CLOSURE -----------------------------------------------------------------

#' Visualize CLOSURE data in a barplot
#'
#' @description `r expand_section("plot_bar_description", "CLOSURE")`
#'
#' @param data `r expand_section("param_data", "CLOSURE")`
#' @param min_max String (length 1). Which plot panel(s) to show? Options are
#'   `"both"` (the default), `"min"`, and `"max"`.
#' @param format String (length 1). What should the bars show? The default is
#'   `"percent"`. Similarly, `"absolute_percent"` shows the count of each scale
#'   value and its percentage of all values. Other options are `"absolute"` and
#'   `"relative"` frequencies.
#' @param samples String (length 1). How to aggregate the samples? Either take
#'   the average sample (`"mean"`, the default) or the sum of all samples
#'   (`"all"`). This only matters if absolute frequencies are shown.
#' @param facet_labels String (length 2). Labels of the two individual panels.
#'   Set it to `NULL` to remove the labels. Default is
#'   `c("Minimal variance", "Maximal variance")`.
#' @param facet_labels_parens String (length 1). Italicized part of the facet
#'   labels inside the parentheses. Set it to `NULL` to remove the parentheses
#'   altogether. See details. Default is `"h"`.
#' @param bar_alpha Numeric (length 1). Opacity of the bars. Default is `0.75`.
#' @param bar_color String (length 1). Color of the bars. Default is
#'   `"#5D3FD3"`, a purple color.
#' @param show_text Logical (length 1). Should the bars be labeled with the
#'   corresponding frequencies? Default is `TRUE`.
#' @param text_color String (length 1). Color of the frequency labels. By
#'   default, the same as `bar_color`.
#' @param text_size Numeric (length 1). Base font size in pt. Default is `12`.
#' @param text_offset Numeric (length 1). Distance between the text labels and
#'   the bars. Default is `0.05`.
#' @param mark_thousand,mark_decimal Strings (length 1 each). Delimiters between
#'   groups of digits in text labels. Defaults are `","` for `mark_thousand`
#'   (e.g., `"20,000"`) and `"."` for `mark_decimal` (e.g., `"0.15"`).
#'
#' @details By default, both faceted plots have a label that includes their
#'   horns index (\eqn{h}); see [`horns()`]. You can remove the parenthesized
#'   part using `facet_labels_parens = NULL` or the entire label using
#'   `facet_labels = NULL`.
#'
#' @return A ggplot object.
#'
#' @seealso [`closure_plot_ecdf()`], an alternative visualization.
#'
#' @include plot-bar-basic.R fn-formals.R
#'
#' @export
#'
#' @examples
#' # Create CLOSURE data first:
#' data <- closure_generate(
#'   mean = "2.9",
#'   sd = "0.5",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Even with minimal and maximal variance,
#' # the results are almost the same:
#' closure_plot_bar(data)
#'
#' # Only show the minimum-variance panel:
#' closure_plot_bar(data, min_max = "min")

# This constructs a function that wraps `plot_frequency_bar()`; see there
closure_plot_bar <- new_plot_fn_bar("CLOSURE", "#5D3FD3")


# SPRITE ------------------------------------------------------------------

#' Visualize SPRITE data in a barplot
#'
#' @description `r expand_section("plot_bar_description", "SPRITE")`
#'
#' @param data `r expand_section("param_data", "SPRITE")`
#' @inheritParams closure_plot_bar
#'
#' @return A ggplot object.
#' @export
#'
#' @examples

# This constructs a function that wraps `plot_frequency_bar()`; see there
sprite_plot_bar <- new_plot_fn_bar("SPRITE", "royalblue1")
