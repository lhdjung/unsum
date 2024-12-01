
#' Visualize CLOSURE data in a histogram
#'
#' @description Call `closure_plot_bar()` to get a barplot of data coming from
#'   [`closure_combine()`], [`closure_pivot_longer()`], or
#'   [`closure_summarize()`].
#'
#'   For each scale value, the bars show how often this value appears in the
#'   full list of possible raw data combinations found by the CLOSURE algorithm.
#'
#' @param data CLOSURE data frame.
#' @param frequency String (length 1). What should the bars display? The
#'   default, `"absolute-percent"`, is to show counts of each scale values
#'   together with their percentages of all values. Other options are
#'   `"absolute"`, `"relative"`, and `"percent"`, but all of them display less
#'   information.
#' @param bar_alpha Numeric (length 1). Opacity of the bars. Default is `0.8`.
#' @param bar_color String (length 1). Color of the bars. Default is
#'   `"royalblue1"`.
#' @param show_text Logical (length 1). Should the bars be labeled with the
#'   corresponding frequencies? Default is `TRUE`.
#' @param text_offset Numeric (length 1). Distance between the text labels and
#'   the bars. Default is `0.05`.
#' @param text_color String (length 1). Color of the text labels. By default,
#'   the same as `bar_color`.
#'
#' @return A ggplot object.
#'
#' @include pivot.R summaries.R
#'
#' @seealso
#' - [`closure_summarize()`], which displays the same information in a
#' data frame.
#' - [`closure_plot_ecdf()`], an alternative visualization.
#'
#' @export
#'
#' @examples
#' # Create CLOSURE data first:
#' data <- closure_combine(
#'   mean = 5.0,
#'   sd = 2.78,
#'   n = 30,
#'   scale_min = 1,
#'   scale_max = 8,
#'   rounding_error_mean = 0.01,
#'   rounding_error_sd = 0.01
#' )
#'
#' # Visualize:
#' closure_plot_bar(data)


# # data <- closure_read("python")
# frequency <- "absolute"
# bar_alpha <- 0.8
# bar_color <- "royalblue1"
# show_text <- TRUE
# text_offset <- 0.05
# text_color <- bar_color

closure_plot_bar <- function(data,
                             frequency = c("absolute-percent",
                                           "absolute",
                                           "relative",
                                           "percent"),
                             bar_alpha = 0.8,
                             bar_color = "royalblue1",
                             show_text = TRUE,
                             text_offset = 0.05,
                             text_color = bar_color) {

  frequency <- rlang::arg_match(frequency)

  # If the data appear to be CLOSURE summary data, verify this more closely.
  # Else, compute the summaries to be visualized. The latter action will check
  # for raw CLOSURE data first, so no such checks are needed here.
  if (inherits(data, "closure_summarize")) {
    check_closure_summarize_unaltered(data)
  } else {
    data <- closure_summarize(data)
  }

  # Create a function that formats labels for large numbers as, e.g., "20,000"
  format_number_label <- scales::label_comma()

  # Remove the column that represents the non-chosen type of frequency, then
  # specify the y-axis label by frequency type.
  if (frequency %in% c("absolute", "absolute-percent")) {
    data$f_relative <- NULL
    sum_absolute <- sum(data$f_absolute)
    label_y_axis <- paste(
      "Count in",
      format_number_label(sum_absolute),
      "values"
    )
    if (frequency == "absolute-percent") {
      label_y_axis <- paste(label_y_axis, "(percentage)")
    }
  } else if (frequency == "relative") {
    data$f_absolute <- NULL
    label_y_axis <- "Relative frequency"
  } else if (frequency == "percent") {
    data$f_absolute <- NULL
    data$f_relative <- 100 * round(data$f_relative, 2)
    label_y_axis <- "Percentage of all values"
  } else {
    cli::cli_abort("Internal error: unhandled `frequency` type.")
  }

  # Ensure consistent column names to be referenced later
  names(data) <- c("value", "frequency")

  # The text geom is pre-defined here because whether it has a non-`NULL` value
  # depends on a logical argument. Also, the text offset is adjusted using the
  # height of the highest bar so that the distance between text and bars is
  # robust to very different values, such as absolute vs. relative values.
  if (show_text) {
    text_offset_adjusted <- text_offset * max(data$frequency)
    needs_label_percent <- frequency %in% c("percent", "absolute-percent")
    if (needs_label_percent && frequency == "absolute-percent") {
      label_percent <- paste0(
        " (", 100 * round(data$frequency / sum(data$frequency), 2), "%)"
      )
    } else if (needs_label_percent) {
      label_percent <- "%"
    } else {
      label_percent <- ""
    }
    geom_text_frequency <- ggplot2::geom_text(
      ggplot2::aes(
        y     = frequency + text_offset_adjusted,
        label = paste0(
          format_number_label(round(frequency, 2)),
          label_percent
        )
      ),
      color = text_color
    )
  } else {
    geom_text_frequency <- NULL
  }

  # Construct the bar plot
  ggplot2::ggplot(data, ggplot2::aes(x = value, y = frequency)) +
    ggplot2::geom_col(alpha = bar_alpha, fill = bar_color) +
    geom_text_frequency +
    ggplot2::scale_x_continuous(breaks = data$value, labels = data$value) +
    ggplot2::scale_y_continuous(
      labels = format_number_label,
      expand = ggplot2::expansion(c(0.01, 0.05))
    ) +
    ggplot2::labs(
      x = "Scale value",
      y = label_y_axis #,
      # title = "CLOSURE: complete listing of original samples of underlying raw evidence"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
      # panel.grid.minor.y = ggplot2::element_blank()
    )

}



#' Visualize CLOSURE data in an ECDF plot
#'
#' @description Call `closure_plot_ecdf()` to visualize data coming from
#'   [`closure_combine()`] or [`closure_pivot_longer()`] using the data's
#'   empirical cumulative distribution function (ECDF).
#'
#'   A diagonal reference line benchmarks the ECDF against a hypothetical linear
#'   relationship.
#'
#'   See [`closure_plot_bar()`] for more intuitive visuals.
#'
#' @details Unlike in [`closure_plot_bar()`], `data` cannot currently be output
#'   of [`closure_summarize()`].
#'
#'   The present function was inspired by [`rsprite2::plot_distributions()`].
#'   However, `plot_distributions()` shows multiple lines because it is based on
#'   SPRITE, which draws random samples of possible datasets. CLOSURE is
#'   exhaustive, so `closure_plot_ecdf()` shows all possible datasets in a
#'   single line.
#'
#' @inheritParams closure_plot_bar
#' @param line_color String (length 1). Color of the ECDF line. Default is
#'   `"royalblue1"`.
#' @param reference_line_alpha Numeric (length 1). Opacity of the diagonal
#'   reference line. Default is `0.6`.
#'
#' @return A ggplot object.
#'
#' @include utils.R closure_pivot_longer.R
#'
#' @export
#'
#' @examples
#' # Create CLOSURE data first:
#' data <- closure_combine(
#'   mean = 5.0,
#'   sd = 2.78,
#'   n = 30,
#'   scale_min = 1,
#'   scale_max = 8,
#'   rounding_error_mean = 0.01,
#'   rounding_error_sd = 0.01
#' )
#'
#' # Visualize:
#' closure_plot_ecdf(data)


# line_color <- "royalblue1"
# reference_line_alpha <- 0.65

closure_plot_ecdf <- function(data,
                              line_color = "royalblue1",
                              reference_line_alpha = 0.6) {

  if (inherits(data$results, "closure_pivot_longer")) {
    check_closure_pivot_longer_unaltered(data)
  } else {
    data <- closure_pivot_longer(data)
  }

  # For the reference line and the x-axis
  values_unique <- closure_summarize(data)$value

  # Construct the ECDF plot
  ggplot2::ggplot(data$results) +
    # ECDF line:
    ggplot2::stat_ecdf(
      ggplot2::aes(value),
      color = line_color,
      pad = TRUE
    ) +
    # Dashed diagonal reference line:
    ggplot2::annotate(
      geom = "segment",
      linetype = 2,
      alpha = reference_line_alpha,
      x = 0,
      xend = max(values_unique),
      y = 0,
      yend = 1
    ) +
    # Rest of the plot:
    ggplot2::labs(x = "Scale value", y = "Cumulative share") +
    ggplot2::scale_x_continuous(
      breaks = values_unique,
      labels = values_unique,
    ) +
    ggplot2::theme_minimal()
}

