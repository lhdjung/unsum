#' Visualize horns index (\eqn{h}) frequencies
#'
#' @description `closure_plot_horns_histogram()` visualizes the distribution of
#'   horns index values found by CLOSURE.
#'
#'   It marks the min, max, and uniform values; see [`horns_uniform()`]. The
#'   x-axis always ranges from 0 to 1. This reveals the big picture, putting any
#'   variability among horns values into perspective.
#'
#' @inheritParams closure_plot_bar
#' @param alpha Numeric (length 1). Opacity of the bars. Default is `0.75`.
#' @param bar_color String (length 1). Color of the bars. Default is
#'   `"#5D3FD3"`, a purple color.
#' @param binwidth Numeric (length 1). Only in `closure_plot_horns_histogram()`.
#'   Width of the bins that divide up the x-axis. Default is `0.01`.
#' @param show_labels String (length 1). Which text labels to show within the
#'   plot? One of:
#'   - `"all"` (the default): *min*, *max*, and *uniform* horns values from
#'   `data`, as well as the *bounds* at 0 and 1.
#'   - `"min_max"`, `"min_max_uniform"`, `"min_max_bounds"`, `"uniform"`,
#'   `"uniform_bounds"`, or `"bounds"`: a subset of these labels.
#'   - `"none"`: no labels.
#' @param line_color_min_max String (length 1). Color of the lines that mark the
#'   lower and upper ends of the distribution. Default is `"red"`.
#' @param line_color_reference String (length 1). Color of the lines at *h = 0*,
#'   *h = 1*, and the uniform point. Default is `"grey20"`.
#' @param text_limits Numeric (length 2). If the minimum horns index is lower
#'   than the first element here (default: `0.12`), both text labels go to the
#'   right of the maximum. The same applies in reverse with the maximum and the
#'   second element (default: `0.88`).
#' @param text_size Numeric (length 1). Base font size in pt. Default is `12`.
#'
#' @name horns-frequency
#' @export
#'
#' @return A ggplot object.
#'
#' @examples
#' data_near_zero <- closure_generate(
#'   mean = "2.1",
#'   sd = "0.4",
#'   n = 35,
#'   scale_min = 1,
#'   scale_max = 7
#' )
#'
#' # All CLOSURE samples have low horns indices:
#' closure_plot_horns_histogram(data_near_zero)
#'
#' data_near_midpoint <- closure_generate(
#'   mean = "4.3",
#'   sd = "2.2",
#'   n = 35,
#'   scale_min = 1,
#'   scale_max = 7
#' )
#'
#' # The min-max lines are beyond the midpoint,
#' # so all CLOSURE samples are bimodal:
#' closure_plot_horns_histogram(data_near_midpoint)
#'
#' data_near_one <- closure_generate(
#'   mean = "4.3",
#'   sd = "2.8",
#'   n = 35,
#'   scale_min = 1,
#'   scale_max = 7
#' )
#'
#' # Extremely high variance throughout; close to the upper limit.
#' # We can omit labels at the bounds so they don't cover the data:
#' closure_plot_horns_histogram(
#'   data_near_one,
#'   show_labels = "min_max_uniform"
#' )
#'
#' # Large differences between horns values occur (only?)
#' # if `mean` and `sd` have no decimal places:
#' data_wide_spread <- closure_generate(
#'   mean = "3",
#'   sd = "2",
#'   n = 35,
#'   scale_min = 1,
#'   scale_max = 7
#' )
#'
#' closure_plot_horns_histogram(data_wide_spread)

closure_plot_horns_histogram <- new_plot_fn_horns_frequency(
  technique = "CLOSURE",
  bar_color = "#5D3FD3"
)
