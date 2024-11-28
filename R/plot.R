
#' Visualize CLOSURE data
#'
#' @description Call `closure_plot()` to get a barplot of data coming from
#'   [`closure_combine()`], [`closure_pivot_longer()`], or
#'   [`closure_summarize()`].
#'
#'   For each scale value, the bars show how often this value appears in the
#'   full list of possible raw data combinations found by the CLOSURE algorithm.
#'
#' @param data CLOSURE data frame.
#' @param frequency String (length 1). What should the bars display? One of
#'   `"absolute"` (the default), `"relative"`, `"percent"`, and
#'   `"absolute-percent"`.
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
#' @seealso [`closure_summarize()`], which displays the same information in a
#'   data frame.
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
#' closure_plot(data)


# # data <- closure_read("python")
# frequency <- "absolute"
# bar_alpha <- 0.8
# bar_color <- "royalblue1"
# show_text <- TRUE
# text_offset <- 0.05
# text_color <- bar_color


closure_plot <- function(data,
                         frequency = c("absolute",
                                       "relative",
                                       "percent",
                                       "absolute-percent"),
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




closure_plot_ecdf <- function(data,
                              line_color = "royalblue1",
                              reference_line_alpha = 0.6) {

  if (inherits(data, "closure_summarize")) {
    check_closure_summarize_unaltered(data)
  } else {
    data <- closure_summarize(data)
  }

  ggplot2::ggplot(data) +
    ggplot2::stat_ecdf(ggplot2::aes(f_absolute), color = line_color) +
    ggplot2::annotate(
      "segment",
      linetype = 2,
      alpha = reference_line_alpha,
      x = 0,
      xend = max(data$f_absolute),
      y = 0,
      yend = 1
    ) +
    ggplot2::labs(x = "Scale value", y = "Cumulative share") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0, 0)) +
    ggplot2::theme_bw()

}

