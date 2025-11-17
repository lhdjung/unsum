#' Visualize horns index (\eqn{h}) frequencies
#'
#' @description Two functions that visualize the distribution of horns index
#'   values found by CLOSURE:
#'
#'   - `closure_plot_horns_histogram()` bins the distribution into categories,
#'   emphasizing individual groups of values. You can adjust this via the
#'   `binwidth` argument.
#'   - `closure_plot_horns_density()` smooths the distribution, emphasizing its
#'   shape. You can use the `density_bounds` argument to avoid apparent density
#'   beyond the limits.
#'
#'   Both mark the minimum and maximum values. The x-axis always ranges from 0
#'   to 1. This reveals the big picture, putting any variability among horns
#'   values into perspective.
#'
#' @inheritParams closure_plot_bar
#' @param alpha Numeric (length 1). Opacity of the density or bars. Default is
#'   `0.75`.
#' @param color String (length 1). Color of the density or bars. Default is
#'   `"#5D3FD3"`, a purple color.
#' @param binwidth Numeric (length 1). Only in `closure_plot_horns_histogram()`.
#'   Width of the bins that divide up the x-axis, passed on to
#' @param density_bounds String (length 1). Only in
#'   `closure_plot_horns_density()`. Which limits, if any, should the density be
#'   forced to fit between? Default is `"none"`. If set to `"min_max"`, this
#'   avoids the illusion of points beyond the limits but can lead to a U-shaped
#'   effect, which would also be misleading. Default is `0.01`.
#' @param line_color_min_max Numeric (length 1). Color of the lines that mark
#'   the lower and upper ends of the distribution. Default is `"red"`.
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
#'   scale_max = 5
#' )
#'
#' closure_plot_horns_histogram(data_near_zero)
#'
#' closure_plot_horns_density(data_near_zero)
#'
#' data_near_midpoint <- closure_generate(
#'   mean = "2.8",
#'   sd = "1.5",
#'   n = 35,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' closure_plot_horns_histogram(data_near_midpoint)
#'
#' closure_plot_horns_density(data_near_midpoint)
#'
#' # Large difference between horns values occur (only?)
#' # if there are no decimal places in `mean` and `sd`:
#' data_wide_spread <- closure_generate(
#'   mean = "3",
#'   sd = "2",
#'   n = 35,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' closure_plot_horns_histogram(data_wide_spread)
#'
#' closure_plot_horns_density(data_wide_spread)

closure_plot_horns_histogram <- new_plot_fn_horns_frequency(
  technique = "CLOSURE",
  type = "histogram"
)


#' @rdname horns-frequency
#' @export

closure_plot_horns_density <- new_plot_fn_horns_frequency(
  technique = "CLOSURE",
  type = "density"
)
