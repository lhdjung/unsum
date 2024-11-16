
#' Visualize CLOSURE data
#'
#' @description Call `closure_plot()` to get a barplot of data coming from
#'   `closure_read()` or [`closure_pivot_longer()`].
#'
#'   For each scale value, the bars show how often this value appears in the
#'   full list of possible raw data combinations found by the CLOSURE technique.
#'
#' @param data CLOSURE data frame.
#' @param frequency String (length 1). What should the bars display? One of
#'   `"absolute"` (the default) and `"relative"`.
#' @param include_text Logical (length 1). Should the bars be labeled with the
#'   corresponding frequencies? Default is `TRUE`.
#' @param text_offset Numeric (length 1). Distance between the text labels and
#'   the bars. Default is `0.05`.
#' @param bar_alpha Numeric (length 1). Opacity of the bars. Default is `0.8`.
#' @param bar_color String (length 1).  Color of the bars. Default is
#'   `"royalblue1"`.
#'
#' @return A ggplot object.
#'
#' @include pivot.R summaries.R
#'
#' @seealso `[closure_count()`] which displays the same information in a data
#'   frame.
#'
#' @export
#'
#' @examples


# frequency <- "relative"
# include_text <- TRUE
# text_offset <- 0.01
# bar_alpha <- 0.8
# bar_color <- "royalblue1"

closure_plot <- function(data,
                         frequency = c("absolute", "relative"),
                         include_text = TRUE,
                         text_offset = 0.05,
                         bar_alpha = 0.8,
                         bar_color = "royalblue1") {

  frequency <- rlang::arg_match(frequency)

  # # Record CLOSURE characteristics, in a way that depends on the data format.
  # if (inherits(data, "closure_pivot_longer")) {
  #
  #   n_all_values <- nrow(data)
  #   unique_n <- unique(data$n)
  #
  #   # Unclear if needed:
  #   n_combinations <- n_all_values / length(unique_n)
  #
  # }  # else

  if (!inherits(data, "closure_pivot_longer")) {

    # n_all_values <- prod(dim(data))
    # unique_n <- seq_len(ncol(data))
    #
    # # Unclear if needed:
    # n_combinations <- nrow(data)

    # Pivot using a bespoke (and exported) wrapper for CLOSURE data
    data <- closure_pivot_longer(data)

  } else if (!inherits(data, "closure_data")) {

    abort_not_closure_data(action = "visualize")

  }

  # Compute the summaries to be visualized
  data <- closure_count(data)

  # Create a function that formats labels for large numbers as, e.g., "20,000"
  format_number_label <- scales::label_comma()

  # Remove the column that represents the non-chosen type of frequency, then
  # specify the y-axis label by frequency type
  if (frequency == "absolute") {
    data$n_relative <- NULL
    n_all_values <- sum(data$n_absolute)
    label_y_axis <- paste(
      "Count in", format_number_label(n_all_values), "values"
    )
  } else if (frequency == "relative") {
    data$n_absolute <- NULL
    label_y_axis <- "Relative frequency"
  } else {
    cli::cli_abort("Internal error: unhandled `frequency` type.")
  }

  # Ensure consistent column names to be referenced later
  names(data) <- c("value", "frequency")

  # The text geom is pre-defined here because whether it has a non-`NULL` value
  # depends on a logical argument. Also, the text offset is adjusted using the
  # height of the highest bar so that the distance between text and bars is
  # robust to very different values, such as absolute vs. relative values.
  if (include_text) {
    text_offset_adjusted <- text_offset * max(data$frequency)
    geom_text_counts <- ggplot2::geom_text(
      ggplot2::aes(
        y     = frequency + text_offset_adjusted,
        label = round(frequency, 2)
      )
    )
  } else {
    geom_text_counts <- NULL
  }

  # Bar plot
  ggplot2::ggplot(data, ggplot2::aes(x = value, y = frequency)) +
    ggplot2::geom_col(alpha = bar_alpha, fill = bar_color) +
    geom_text_counts +
    ggplot2::scale_x_continuous(breaks = data$value, labels = data$value) +
    ggplot2::scale_y_continuous(
      labels = format_number_label,
      expand = ggplot2::expansion(c(0.01, 0.1))
    ) +
    ggplot2::labs(
      x = "Scale value",
      y = label_y_axis
      # title = "CLOSURE: complete listing of original samples of underlying raw evidence"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
      # panel.grid.minor.y = ggplot2::element_blank()
    )

}
