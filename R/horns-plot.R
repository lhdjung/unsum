#' Visualize horns values distribution
#'
#' @description Two functions that follow up on [`closure_horns_analyze()`]:
#' - `closure_horns_min_max_bar()` draws barplots of the mean samples from among
#'   those with the minimum or maximum horns index. It displays the typical
#'   sample with the least or most amount of variability from among all CLOSURE
#'   samples.
#' - `closure_horns_histogram()` draws a quick barplot of the distribution of
#'   horns values. The scale is fixed between 0 and 1, so it is aligned with the
#'   range of the horns index. This reveals the big picture, putting any
#'   variability among horns values into perspective.
#'
#' @param data List returned by [`closure_horns_analyze()`].
#' @param min_max String (length 1). Only in `closure_horns_min_max_bar()`.
#'   Which plot(s) to show? Options are `"both"` (the default), `"min"`, and
#'   `"max"`.
#' @param facet_labels String (length 2). Only in `closure_horns_min_max_bar()`.
#'   Labels of the two individual plots. Default is `c("Minimal variability",
#'   "Maximal variability")`.
#' @param facet_labels_parens String (length 1). Italicized part of the facet
#'   labels inside the parentheses. Set it to `NULL` to remove the parentheses
#'   altogether. Default is `"h"`.
#' @param bar_binwidth Numeric (length 1). Only in `closure_horns_histogram()`.
#'   Width of the bins that divide up the x-axis, passed on to
#'   [`ggplot2::geom_histogram()`]. Default is `0.0025`.
#' @inheritParams closure_plot_bar
#'
#' @name horns_plot
#'
#' @examples
#' # Preparation (1 / 2): run CLOSURE
#' data <- closure_generate(
#'   mean = "2.9",
#'   sd = "0.5",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Preparation (2 / 2): analyze sample-wise horns values
#' data_horns <- closure_horns_analyze(data)
#'
#' # Even with minimal and maximal variability,
#' # the results are almost the same:
#' closure_horns_min_max_bar(data_horns)
#'
#' # They cluster in a narrow slice of the 0-1 range
#' # of the horns index:
#' closure_horns_histogram(data_horns)


#' @rdname horns_plot
#' @export

# Arguments for this function are generated below the definition
closure_horns_min_max_bar <- function() {
  check_closure_horns_analyze(data)

  check_length(facet_labels, 2L)
  check_length(facet_labels_parens, 1L, allow_null = TRUE)

  min_max <- rlang::arg_match(min_max)

  names_min_max <- c(
    "frequency_horns_min",
    "frequency_horns_max"
  )

  plot_frequency_bar(
    data = data,
    frequency = frequency,
    samples = samples,
    min_max_values = c(
      data$horns_metrics$min,
      data$horns_metrics$max
    ),
    name_frequency_table = switch(
      min_max,
      "both" = names_min_max,
      "min" = names_min_max[1L],
      "max" = names_min_max[2L]
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

formals(closure_horns_min_max_bar) <- plot_frequency_bar |>
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
    "name_frequency_table"
  )


#' @rdname horns_plot
#' @export

closure_horns_histogram <- function(
    data,
    bar_alpha = 0.75,
    bar_color = "#5D3FD3",
    bar_binwidth = 0.0025,
    text_size = 12
) {

  check_closure_horns_analyze(data)

  data <- data$horns_results

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
      x = "Horns index (h)",
      y = "Count in all horns index values"
    ) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}

