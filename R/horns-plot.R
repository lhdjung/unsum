#' Visualize horns values distribution
#'
#' @description Two plot functions that follow up on [`closure_generate()`]:
#' - `closure_plot_bar_min_max()` draws barplots of the mean samples from among
#'   those with the minimum or maximum horns index (\eqn{h}). It displays the
#'   typical sample with the least or most amount of variability from among all
#'   CLOSURE samples.
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
#' @include plot-basic.R
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
    "technique",
    "min_max_values",
    "frequency_rows_subset"
  )

# Internal basis of the `*_plot_horns_*()` functions
plot_horns_frequency <- function(
  data,
  type,
  technique,
  alpha = 0.75,
  color = "#5D3FD3",
  binwidth = 0.01,
  density_limits = c("none", "min_max"),
  line_color_min_max = "red",
  line_color_uniform = "grey20",
  text_limits = c(0.12, 0.88),
  text_size = 12,
  mark_decimal = "."
) {
  check_generator_output(data, technique)

  check_length(text_limits, 2L)

  density_limits <- rlang::arg_match(density_limits)

  h_min <- data$metrics_horns$min
  h_max <- data$metrics_horns$max
  h_uniform <- data$metrics_horns$uniform

  # Reduce the input to a tibble that only includes the horns values
  data <- data$results["horns"]

  # Position labels: min label to the left, max label to the right. Add a small
  # horizontal offset from the line.
  label_offset <- 0.02

  position_x_min <- max(0, h_min - label_offset)
  position_x_max <- min(1, h_max + label_offset)

  # Can't go too low because `h_uniform` >= 1 / 3 which leaves enough space for
  # the text label
  position_x_uniform <- min(1, h_uniform - 0.1)

  # Left-align min label, right-align max label
  hjust_min <- 1
  hjust_max <- 0

  vjust_min <- 2
  vjust_max <- 2

  # If the minimum horns value is too close to 0 for the min label to fit on its
  # left, move the max label a bit lower and the min label above it. The same
  # applies vice versa with 1 and the maximum value.
  if (h_min < text_limits[1]) {
    position_x_min <- position_x_max
    hjust_min <- hjust_max
    vjust_max <- 3.5
  } else if (h_max > text_limits[2]) {
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
    mark_decimal = mark_decimal
  )

  label_uniform <- format_equation(
    prefix = "Uniform",
    number = h_uniform,
    var_name = "h",
    mark_decimal = mark_decimal
  )

  # Plot type determines geom to be shown
  geom_frequency <- switch(
    type,
    "density" = ggplot2::stat_density(
      ggplot2::aes(x = .data$horns),
      alpha = alpha,
      fill = color,
      color = color,
      linewidth = 0.5,
      bw = 0.005,
      # `NULL` if not matched
      bounds = switch(
        density_limits,
        "min_max" = c(h_min, h_max)
      )
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

    # Min and max reference lines
    ggplot2::geom_vline(
      xintercept = c(h_min, h_max),
      linetype = 2,
      alpha = 0.75,
      color = line_color_min_max,
      linewidth = 0.75
    ) +
    # Uniform reference line
    ggplot2::geom_vline(
      xintercept = h_uniform,
      color = line_color_uniform,
    ) +

    # Text label for min and max lines
    ggplot2::annotate(
      geom = "label",
      x = c(position_x_min, position_x_max),
      y = Inf,
      label = label_min_max,
      vjust = c(vjust_min, vjust_max),
      hjust = c(hjust_min, hjust_max),
      color = line_color_min_max,
      fill = "white",
      parse = TRUE
    ) +
    # Text label for uniform line
    ggplot2::annotate(
      geom = "label",
      x = h_uniform,
      y = -Inf,
      label = label_uniform,
      vjust = -4.5,
      hjust = 0.5,
      color = line_color_uniform,
      fill = "white",
      parse = TRUE
    ) +

    # Rest of the plot
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
