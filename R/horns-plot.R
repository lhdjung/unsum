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
#' @param bar_binwidth Numeric (length 1). Only in
#'   `closure_plot_horns_histogram()`. Width of the bins that divide up the
#'   x-axis, passed on to [`ggplot2::geom_histogram()`]. Default is `0.0025`.
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


#' @rdname horns_plot
#' @export

closure_plot_horns_histogram <- function(
  data,
  bar_alpha = 0.75,
  bar_color = "#5D3FD3",
  bar_binwidth = 0.0025,
  text_size = 12
) {
  check_closure_generate(data)

  # Reduce the input to a tibble that only includes the horns values
  data <- data$results["horns"]

  # Construct the plot
  ggplot2::ggplot(data) +
    ggplot2::geom_histogram(
      ggplot2::aes(x = horns),
      alpha = bar_alpha,
      fill = bar_color,
      binwidth = bar_binwidth
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      oob = function(x, limits) x
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
