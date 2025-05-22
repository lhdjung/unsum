
#' Visualize CLOSURE data in a histogram
#'
#' @description Call `closure_plot_bar()` to get a barplot of CLOSURE results.
#'
#'   For each scale value, the bars show how often this value appears in the
#'   full list of possible raw data combinations found by the CLOSURE algorithm.
#'
#' @param data List returned by [`closure_combine()`].
#' @param frequency String (length 1). What should the bars display? The
#'   default, `"absolute-percent"`, displays the counts of each scale value, and
#'   if text labels are shown (by default of `show_text = TRUE`), its percentage
#'   of all values. Other options are `"absolute"`, `"relative"`, and
#'   `"percent"`.
#' @param samples String (length 1). Should the plot show the sum of all values
#'   (`"all"`, the default) or the average sample (`"average"`)? This only
#'   matters if absolute frequencies are shown.
#' @param bar_alpha Numeric (length 1). Opacity of the bars. Default is `0.8`.
#' @param bar_color String (length 1). Color of the bars. Default is
#'   `"royalblue1"`.
#' @param show_text Logical (length 1). Should the bars be labeled with the
#'   corresponding frequencies? Default is `TRUE`.
#' @param text_color String (length 1). Color of the frequency labels. By
#'   default, the same as `bar_color`.
#' @param text_size Numeric. Base font size in pt. Default is `13`.
#' @param text_offset Numeric (length 1). Distance between the text labels and
#'   the bars. Default is `0.05`.
#' @param mark_thousand,mark_decimal Strings (length 1 each). Delimiters between
#'   groups of digits in text labels. Defaults are `","` for `mark_thousand`
#'   (e.g., `"20,000"`) and `"."` for `mark_decimal` (e.g., `"0.15"`).
#'
#' @return A ggplot object.
#'
#' @seealso [`closure_plot_ecdf()`], an alternative visualization.
#'
#' @export
#'
#' @examples
#' # Create CLOSURE data first:
#' data <- closure_combine(
#'   mean = "3.5",
#'   sd = "2",
#'   n = 52,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Visualize:
#' closure_plot_bar(data)


# TODO: Consider using bar_color = "#4e004f", "#52003a", "#610019", "#341d5c" or
# similar to distinguish CLOSURE plots from SPRITE plots; especially if and when
# SPRITE gets implemented in unsum!

# # For interactive testing:
# # (create `data`)
# frequency <- "absolute"
# samples <- "all"
# bar_alpha <- 0.8
# bar_color <- "royalblue1"
# show_text <- TRUE
# text_offset <- 0.05
# text_color <- bar_color
# mark_thousand <- ","
# mark_decimal <- "."
# text_size <- 13

closure_plot_bar <- function(data,
                             frequency = c("absolute-percent",
                                           "absolute",
                                           "relative",
                                           "percent"),
                             # TODO: Which one should be the default here -- all
                             # samples or the average sample?
                             samples = c("all", "average"),
                             bar_alpha = 0.8,
                             bar_color = "royalblue1",
                             show_text = TRUE,
                             text_color = bar_color,
                             text_size = 13,
                             text_offset = 0.05,
                             mark_thousand = ",",
                             mark_decimal = ".") {

  # Check inputs
  check_closure_combine(data)
  frequency <- rlang::arg_match(frequency)
  samples <- rlang::arg_match(samples)

  # Zoom in on the frequency table -- the only element of `data` needed here
  data <- data$frequency

  # In terms of absolute frequencies, the user may choose to show the average
  # number of observations per bin instead of the full count. If so, replace the
  # full absolute values by the average values, and prepare a label to signpost
  # the average inside of the plot.
  if (samples == "average") {
    # Warn if the user chooses relative frequencies but also averaging. This
    # won't matter for a Shiny app where users keep buttons checked.
    if (interactive() && frequency %in% c("relative", "percent")) {
      cli::cli_warn(c(
        "Averaging samples only matters to absolute frequencies.",
        "!" = "Using `samples = \"average\"` here has no effect because of \
        `frequency = \"{frequency}\"`."
      ))
    }
    data$f_absolute <- data$f_average
    label_avg_all <- "avg. sample, N = "
    label_values <- " "
  } else {
    label_avg_all <- "all "
    label_values <- " values "
  }

  # After that, the average is not needed in any case, even if it was before
  data$f_average <- NULL

  # Create a function that formats labels for large numbers. By default, they
  # are formatted like, e.g., "12,345.67"
  format_number_label <- scales::label_number(
    big.mark = mark_thousand,
    decimal.mark = mark_decimal
  )

  # Remove the column that represents the main non-chosen type of frequency
  # (absolute or relative), then specify the y-axis label by frequency type.
  if (frequency %in% c("absolute", "absolute-percent")) {
    label_y_axis <- paste0(
      "Count in ",
      label_avg_all,
      format_number_label(sum(data$f_absolute)),
      label_values
    )
    if (frequency == "absolute-percent") {
      label_y_axis <- paste0(label_y_axis, "(%)")
    }
    data$f_relative <- NULL
  } else if (frequency == "relative") {
    label_y_axis <- "Relative frequency"
    data$f_absolute <- NULL
  } else if (frequency == "percent") {
    label_y_axis <- "Percentage of all values"
    data$f_relative <- 100 * round(data$f_relative, 2)
    data$f_absolute <- NULL
  } else {
    cli::cli_abort("Internal error: unhandled `frequency` type.")
  }

  # Ensure consistent column names to be referenced later
  names(data) <- c("value", "frequency")

  # The text geom is pre-defined here because whether it has a non-`NULL` value
  # depends on a logical argument.
  if (show_text) {
    # Prepare a percent label if the frequency display should be a percentage,
    # at least in part. Otherwise, `label_percent` is `NULL`, so it's ignored.
    label_percent <- switch(
      frequency,
      "absolute-percent" = paste0(
        " (",
        100 * round(data$frequency / sum(data$frequency), 2),
        "%)"
      ),
      "percent" = "%"
    )
    # Adjust the text offset using the height of the highest bar so that the
    # distance between text and bars is robust to very different values, such as
    # absolute vs. relative values.
    text_offset_adjusted <- text_offset * max(data$frequency)
    geom_text_frequency <- ggplot2::geom_text(
      ggplot2::aes(
        y = frequency + text_offset_adjusted,
        label = paste0(
          format_number_label(round(frequency, 2)),
          label_percent
        )
      ),
      color = text_color,
      size = text_size / 3
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
      y = label_y_axis
    ) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )

}



#' Visualize CLOSURE data in an ECDF plot
#'
#' @description Call `closure_plot_ecdf()` to visualize CLOSURE results using
#'   the data's empirical cumulative distribution function (ECDF).
#'
#'   A diagonal reference line benchmarks the ECDF against a hypothetical linear
#'   relationship.
#'
#'   See [`closure_plot_bar()`] for more intuitive visuals.
#'
#' @details The present function was inspired by
#'   [`rsprite2::plot_distributions()`]. However, `plot_distributions()` shows
#'   multiple lines because it is based on SPRITE, which draws random samples of
#'   possible datasets. CLOSURE is exhaustive, so `closure_plot_ecdf()` shows
#'   all possible datasets in a single line.
#'
#' @param line_color String (length 1). Color of the ECDF line. Default is
#'   `"royalblue1"`.
#' @param reference_line_alpha Numeric (length 1). Opacity of the diagonal
#'   reference line. Default is `0.6`.
#' @param pad Logical (length 1). Should the ECDF line be padded on the x-axis
#'   so that it stretches beyond the data points? Default is `TRUE`.
#' @inheritParams closure_plot_bar
#'
#' @return A ggplot object.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # Create CLOSURE data first:
#' data <- closure_combine(
#'   mean = "3.5",
#'   sd = "2",
#'   n = 52,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Visualize:
#' closure_plot_ecdf(data)


# # For interactive testing:
# # (create `data`)
# line_color <- "royalblue1"
# text_size <- 13
# reference_line_alpha <- 0.6
# pad <- TRUE


closure_plot_ecdf <- function(data,
                              line_color = "royalblue1",
                              text_size = 13,
                              reference_line_alpha = 0.6,
                              pad = TRUE) {

  check_closure_combine(data)

  # For the reference line and the x-axis
  inputs <- data$inputs
  metrics <- data$metrics
  values_unique <- inputs$scale_min:inputs$scale_max

  # Zoom in on the detailed `results` -- the key element of `data` needed here.
  # Flatten them into a single integer vector.
  data <- tibble::new_tibble(
    x = list(
      value = unlist(data$results$combination, use.names = FALSE)
    ),
    nrow = metrics$values_all
  )

  # Construct the ECDF plot
  ggplot2::ggplot(data, ggplot2::aes(value)) +
    # ECDF line:
    ggplot2::stat_ecdf(
      color = line_color,
      pad = pad
    ) +
    # Dashed diagonal reference line:
    ggplot2::annotate(
      geom = "segment",
      linetype = 2,
      alpha = reference_line_alpha,
      x = values_unique[1],
      xend = values_unique[length(values_unique)],
      y = 0,
      yend = 1
    ) +
    # Rest of the plot:
    ggplot2::labs(x = "Scale value", y = "Cumulative share") +
    ggplot2::scale_x_continuous(
      breaks = values_unique,
      labels = values_unique,
    ) +
    ggplot2::theme_minimal(base_size = text_size)
}

