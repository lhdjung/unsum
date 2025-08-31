#' Visualize horns values distribution
#'
#' @description Two plot functions that follow up on [`closure_generate()`]:
#' - `closure_plot_bar_min_max()` draws barplots of the mean samples from among
#'   those with the minimum or maximum horns index (\eqn{h}). It displays the
#'   typical sample with the least or most amount of variability from among all
#'   CLOSURE samples.
#' - `closure_plot_horns_histogram()` draws a quick barplot of the distribution
#'   of horns values. The scale is fixed between 0 and 1, so it is aligned with
#'   the range of the horns index. This reveals the big picture, putting any
#'   variability among horns values into perspective.
#'
#' @param data List returned by [`closure_generate()`].
#' @param min_max String (length 1). Only in `closure_plot_bar_min_max()`. Which
#'   plot(s) to show? Options are `"both"` (the default), `"min"`, and `"max"`.
#' @param facet_labels String (length 2). Only in `closure_plot_bar_min_max()`.
#'   Labels of the two individual plots. Set it to `NULL` to remove the labels.
#'   Default is `c("Minimal variability", "Maximal variability")`.
#' @param facet_labels_parens String (length 1). Only in
#'   `closure_plot_bar_min_max()`. Italicized part of the facet labels inside
#'   the parentheses. Set it to `NULL` to remove the parentheses altogether. See
#'   details. Default is `"h"`.
#' @param binwidth Numeric (length 1). Only in
#'   `closure_plot_horns_histogram()`. Width of the bins that divide up the
#'   x-axis, passed on to [`ggplot2::geom_histogram()`]. Default is `0.01`.
#' @inheritParams closure_plot_bar
#'
#' @details By default, both faceted plots in `closure_plot_bar_min_max()` have
#'   a label that includes their horns index (\eqn{h}); see [`horns()`]. You can
#'   remove the parenthesized part using `facet_labels_parens = NULL` or the
#'   entire label using `facet_labels = NULL`.
#'
#'   Although `facet_labels_parens` enables you to choose a different string
#'   inside the parentheses than the default `"h"`, this might not be advisable:
#'   if the parentheses are present, they will always display the horns index.
#'
#' @name horns_plot
#'
#' @include plot.R
#'
#' @return A ggplot object.
#'
#' @examples
#' # Preparation: run CLOSURE
#' data <- closure_generate(
#'   mean = "2.9",
#'   sd = "0.5",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Even with minimal and maximal variability,
#' # the results are almost the same:
#' closure_plot_bar_min_max(data)
#'
#' # They cluster in a narrow slice of the 0-1 range
#' # of the horns index:
#' closure_plot_horns_histogram(data)

#' @rdname horns_plot
#' @export

# min_max = "both"
# frequency = "absolute-percent"
# samples = "mean"
# facet_labels = c("Minimal variability", "Maximal variability")
# facet_labels_parens = "h"
# bar_alpha = 0.75
# bar_color = "#5D3FD3"
# show_text = TRUE
# text_color = bar_color
# text_size = 12
# text_offset = 0.05
# mark_thousand = ","
# mark_decimal = "."

# Arguments for this function are generated below the definition
closure_plot_bar_min_max <- function() {
  check_length(facet_labels, 2L, allow_null = TRUE)
  check_length(facet_labels_parens, 1L, allow_null = TRUE)

  min_max <- rlang::arg_match(min_max)

  # Enable plots without facet labels
  if (is.null(facet_labels)) {
    facet_labels <- rep("\"\"", 2)
    facet_labels_parens <- NULL
  }

  plot_frequency_bar(
    data = data,
    frequency = frequency,
    samples = samples,
    min_max_values = c(
      data$metrics_horns$min,
      data$metrics_horns$max
    ),
    frequency_rows_subset = switch(
      min_max,
      "both" = c("horns_min", "horns_max"),
      "min" = "horns_min",
      "max" = "horns_max"
    ),
    facet_labels = switch(
      min_max,
      "both" = facet_labels,
      "min" = facet_labels[1L],
      "max" = facet_labels[2L]
    ),
    facet_labels_parens = facet_labels_parens,
    bar_alpha = bar_alpha,
    bar_color = bar_color,
    show_text = show_text,
    text_color = text_color,
    text_size = text_size,
    text_offset = text_offset,
    mark_thousand = mark_thousand,
    mark_decimal = mark_decimal
  )
}

formals(closure_plot_bar_min_max) <- plot_frequency_bar |>
  formals() |>
  formals_change_defaults(
    bar_color = "#5D3FD3"
  ) |>
  formals_change_defaults(
    facet_labels = c("Minimal variability", "Maximal variability")
  ) |>
  formals_change_defaults(
    facet_labels_parens = "h"
  ) |>
  formals_add(
    min_max = c("both", "min", "max"),
    .after = "data"
  ) |>
  formals_remove(
    "min_max_values",
    "frequency_rows_subset"
  )


#' Visualize horns index (\eqn{h}) frequencies
#'
#' @description Two functions that visualize the distribution of horns index
#'   values found by CLOSURE:
#'
#'   - `closure_plot_horns_density()` smooths the distribution, emphasizing its
#'   shape. This is generally recommended.
#'   - `closure_plot_horns_histogram()` bins the distribution into categories,
#'   emphasizing individual groups of values. You can adjust this via the
#'   `binwidth` argument.
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
#'   [`ggplot2::geom_histogram()`]. Default is `0.01`.
#' @param text_size Numeric (length 1). Base font size in pt. Default is `12`.
#'
#' @name horns-frequency
#' @export

closure_plot_horns_density <- function(
  data,
  alpha = 0.75,
  color = "#5D3FD3",
  reference_line_color = "red",
  text_size = 12
) {
  plot_horns_frequency(
    data = data,
    type = "density",
    alpha = alpha,
    color = color,
    reference_line_color = reference_line_color,
    text_size = text_size
  )
}

#' @rdname horns-frequency
#' @export

closure_plot_horns_histogram <- function(
  data,
  binwidth = 0.01,
  alpha = 0.75,
  color = "#5D3FD3",
  reference_line_color = "red",
  text_size = 12
) {
  plot_horns_frequency(
    data = data,
    type = "histogram",
    alpha = alpha,
    color = color,
    binwidth = binwidth,
    reference_line_color = reference_line_color,
    text_size = text_size
  )
}


# Internal basis of the `*_plot_horns_*()` functions
plot_horns_frequency <- function(
  data,
  type,
  alpha,
  color,
  binwidth,
  reference_line_color,
  text_size
) {
  check_closure_generate(data)

  h_min <- data$metrics_horns$min
  h_max <- data$metrics_horns$max

  # Reduce the input to a tibble that only includes the horns values
  data <- data$results["horns"]

  # Position labels: min label to the left, max label to the right. Add a small
  # horizontal offset from the line.
  label_offset <- 0.02

  position_x_min <- max(0, h_min - label_offset)
  position_x_max <- min(1, h_max + label_offset)

  # Left-align min label, right-align max label
  hjust_min <- 1
  hjust_max <- 0

  vjust_min <- 2
  vjust_max <- 2

  # If the minimum horns value is too close to 0 for the min label to fit on its
  # left, move the max label a bit lower and the min label above it. The same
  # applies vice versa with 1 and the maximum value.
  if (h_min < 0.12) {
    position_x_min <- position_x_max
    hjust_min <- hjust_max
    vjust_max <- 3.5
  } else if (h_max > 0.88) {
    position_x_max <- position_x_min
    hjust_max <- hjust_min
    vjust_max <- 3.5
  }

  # String with labels such as "Min (h = 0.68)" and "Max (h = 0.75)"; where "h"
  # is in italics. Not parsing as an expression here because `annotate()` will
  # do that itself, and parsing here would lead to a spurious warning.
  label_min_max <- format_equation(
    prefix = c("Min", "Max"),
    number = c(h_min, h_max),
    var_name = "h",
    mark_decimal = ".",
    parse_output = FALSE
  )

  geom_frequency <- switch(
    type,
    "density" = ggplot2::stat_density(
      ggplot2::aes(x = .data$horns),
      alpha = alpha,
      fill = color,
      color = color,
      linewidth = 0.5,
      bw = 0.005
    ),
    "histogram" = ggplot2::geom_histogram(
      ggplot2::aes(x = .data$horns),
      alpha = alpha,
      fill = color,
      binwidth = binwidth
    ),
    cli::cli_abort("Internal error: unhandled plot type.")
  )

  # Construct the plot
  ggplot2::ggplot(data) +
    geom_frequency +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      oob = function(x, limits) x
    ) +
    ggplot2::geom_vline(
      xintercept = c(h_min, h_max),
      linetype = 2,
      alpha = 0.75,
      color = reference_line_color,
      size = 0.75
    ) +
    ggplot2::annotate(
      geom = "text",
      x = c(position_x_min, position_x_max),
      y = Inf,
      label = label_min_max,
      vjust = c(vjust_min, vjust_max),
      hjust = c(hjust_min, hjust_max),
      color = "black",
      parse = TRUE
    ) +
    ggplot2::labs(
      x = expression(paste("Horns index (", italic("h"), ")")),
      y = expression(paste("Count in all ", italic("h"), " values"))
    ) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}
