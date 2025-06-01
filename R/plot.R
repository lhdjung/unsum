#' Visualize CLOSURE data in a histogram
#'
#' @description Call `closure_plot_bar()` to get a barplot of CLOSURE results.
#'
#'   For each scale value, the bars show how often this value appears in the
#'   full list of possible raw data samples found by the CLOSURE algorithm.
#'
#' @param data List returned by [`closure_generate()`].
#' @param frequency String (length 1). What should the bars display? The
#'   default, `"absolute-percent"`, displays the count of each scale value and
#'   its percentage of all values. Other options are `"absolute"`, `"relative"`,
#'   and `"percent"`.
#' @param samples String (length 1). How to aggregate the samples? Either take
#'   the average sample (`"mean"`, the default) or the sum of all samples
#'   (`"all"`). This only matters if absolute frequencies are shown.
#' @param bar_alpha Numeric (length 1). Opacity of the bars. Default is `0.75`.
#' @param bar_color String (length 1). Color of the bars. Default is
#'   `"#5D3FD3"`, a purple color.
#' @param show_text Logical (length 1). Should the bars be labeled with the
#'   corresponding frequencies? Default is `TRUE`.
#' @param text_color String (length 1). Color of the frequency labels. By
#'   default, the same as `bar_color`.
#' @param text_size Numeric. Base font size in pt. Default is `12`.
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
#' data <- closure_generate(
#'   mean = "3.5",
#'   sd = "2",
#'   n = 52,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Visualize:
#' closure_plot_bar(data)

# TODO: Consider using bar_color = "#4e004f", "#52003a", "#610019", "#880808",
# "#341d5c" or similar to distinguish CLOSURE plots from SPRITE plots;
# especially if and when SPRITE gets implemented in unsum!

# # For interactive testing:
# # (create `data`)
# frequency <- "absolute-percent"
# samples <- "mean"
# bar_alpha <- 0.75
# bar_color <- "#960019"
# show_text <- TRUE
# text_offset <- 0.05
# text_color <- bar_color
# mark_thousand <- ","
# mark_decimal <- "."
# text_size <- 12

closure_plot_bar <- function(
  data,
  frequency = c("absolute-percent", "absolute", "relative", "percent"),
  # TODO: Which one should be the default here -- all
  # samples or the average sample?
  samples = c("mean", "all"),
  bar_alpha = 0.75,
  # TODO: Choose favorite -- #880808, #960019, #5D3FD3
  bar_color = "#5D3FD3",
  show_text = TRUE,
  text_color = bar_color,
  text_size = 12,
  text_offset = 0.05,
  mark_thousand = ",",
  mark_decimal = "."
) {

  # Check inputs
  check_closure_generate(data)
  frequency <- rlang::arg_match(frequency)
  samples <- rlang::arg_match(samples)

  # Zoom in on the frequency table -- the only element of `data` needed here
  data <- data$frequency

  # In terms of absolute frequencies, the user may choose to show the average
  # number of observations per bin instead of the full count. If so, replace the
  # full absolute values by the average values, and prepare a label to signpost
  # the average inside of the plot.
  if (samples == "mean") {
    data$f_absolute <- data$f_average
    label_avg_all <- "avg. sample, N = "
    label_values <- " "
  } else if (samples == "all") {
    label_avg_all <- "all "
    label_values <- " values "
  } else {
    cli::cli_abort("Internal error: unhandled `samples` type.")
  }

  # After that, the average is not needed in any case, even if it was before
  data$f_average <- NULL

  # Create a function that formats labels for large numbers. By default, they
  # are formatted like, e.g., "12,345.67". In terms of `accuracy`, relative
  # numbers deserve two decimal places because such nuance matters for fractions
  # of 1. If the mean sample or the percentage of values is shown, this is less
  # important, but one decimal place might matter, and it can show that the
  # numbers are fractions. However, if all numbers are absolute integers, there
  # is no need for decimal places.
  format_number_label <- scales::label_number(
    accuracy = if (frequency == "relative") {
      0.01
    } else if (samples == "mean" || frequency == "percent") {
      0.1
    } else {
      1
    },
    big.mark = mark_thousand,
    decimal.mark = mark_decimal
  )

  # Remove the column that represents the main non-chosen type of frequency
  # (absolute or relative) after specifying the y-axis label by frequency type.
  if (frequency %in% c("absolute", "absolute-percent")) {
    label_y_axis <- paste0(
      "Count in ",
      label_avg_all,
      # Need to create a separate number-formatting function on the fly to
      # format the number of values found by CLOSURE: no decimal places but a
      # comma (or similar) to separate levels of thousand every three digits.
      scales::label_number(
        accuracy = 1,
        big.mark = mark_thousand
      )(sum(data$f_absolute)),
      label_values,
      if (frequency == "absolute-percent") "(%)" else NULL
    )
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
          format_number_label(frequency),
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
#'   all possible datasets in a single line by default.
#'
#' @param samples String (length 1). How to aggregate the samples? Either draw a
#'   single ECDF line for the average sample (`"mean"`, the default); or draw a
#'   separate line for each sample (`"all"`). Note: the latter option can be
#'   very slow if many values were found.
#' @param line_color String (length 1). Color of the ECDF line. Default is
#'   `"#5D3FD3"`, a purple color.
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
#' data <- closure_generate(
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
# samples <- "all"
# line_color <- "#960019"
# text_size <- 12
# reference_line_alpha <- 0.6
# pad <- TRUE

closure_plot_ecdf <- function(
  data,
  samples = c("mean", "all"),
  line_color = "#5D3FD3",
  text_size = 12,
  reference_line_alpha = 0.6,
  pad = TRUE
) {

  check_closure_generate(data)
  samples <- rlang::arg_match(samples)

  # For the reference line and the x-axis
  inputs <- data$inputs
  metrics <- data$metrics
  values_unique <- inputs$scale_min:inputs$scale_max

  # Zoom in on the detailed `results` -- the key element of `data` needed here.
  # Flatten them into a single integer vector. If all samples should be shown,
  # enable grouping the values by sample using a `sample_id` column.
  data <- tibble::new_tibble(
    x = list(
      value = unlist(data$results$sample, use.names = FALSE),
      sample_id = if (samples == "all") {
        rep(seq_len(metrics$samples_all), each = inputs$n)
      } else {
        NULL
      }
    ),
    nrow = metrics$values_all
  )

  # Prepare the geom-like ggplot2 object that maps the data to the ECDF line(s).
  # Group the atomic integer values by `sample_id` if needed.
  if (samples == "mean") {
    stat_ecdf_line <- ggplot2::stat_ecdf(
      color = line_color,
      pad = pad
    )
  } else if (samples == "all") {
    stat_ecdf_line <- ggplot2::stat_ecdf(
      ggplot2::aes(
        group = .data$sample_id,
        color = as.factor(.data$sample_id)
      ),
      pad = pad
    )
  } else {
    cli::cli_abort("Internal error: unhandled `samples` type.")
  }

  # Construct the ECDF plot
  ggplot2::ggplot(data, ggplot2::aes(value)) +
    # ECDF line:
    stat_ecdf_line +
    # Dashed diagonal reference line:
    ggplot2::annotate(
      geom = "segment",
      linetype = 2,
      alpha = reference_line_alpha,
      x = 0,
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
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(legend.position = "none")
}
