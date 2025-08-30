# TODO: Consider using bar_color = "#4e004f", "#52003a", "#610019", "#880808",
# "#341d5c" or similar to distinguish CLOSURE plots from SPRITE plots;
# especially if and when SPRITE gets implemented in unsum!

# # For interactive testing:
# data <- closure_generate(
#   mean = "3.5",
#   sd = "1.7",
#   n = 70,
#   scale_min = 1,
#   scale_max = 5
# )
# frequency <- "absolute-percent"
# samples <- "mean"
# bar_alpha <- 0.75
# bar_color <- "#5D3FD3"
# show_text <- TRUE
# text_offset <- 0.05
# text_color <- bar_color
# mark_thousand <- ","
# mark_decimal <- "."
# text_size <- 12
# frequency_rows_subset <- c("horns_min", "horns_max")
# facet_labels <- c("Minimal variability", "Maximal variability")
# facet_labels_parens <- "h"
# min_max_values <- c(0.41, 0.42)

plot_frequency_bar <- function(
  data,
  frequency = c("absolute-percent", "absolute", "relative", "percent"),
  samples = c("mean", "all"),
  min_max_values = NULL,
  frequency_rows_subset = NULL,
  facet_labels = NULL,
  facet_labels_parens = NULL,
  bar_alpha = 0.75,
  bar_color = "black",
  show_text = TRUE,
  text_color = bar_color,
  text_size = 12,
  text_offset = 0.05,
  mark_thousand = ",",
  mark_decimal = "."
) {
  # Check inputs
  frequency <- rlang::arg_match(frequency)
  samples <- rlang::arg_match(samples)

  # Zoom in on the frequency table -- the only element of `data` needed here.
  # Filter its rows to only keep those with a specific subset of samples, such
  # as "horns_min" and "horns_max", or "all" for all samples taken together.
  data <- data$frequency
  data <- data[data$samples %in% frequency_rows_subset, ]

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
      )(sum(data$f_absolute) / length(unique(data$samples))),
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
  names(data)[!(names(data) %in% c("samples", "group_frequency_table"))] <- c(
    "value",
    "frequency"
  )

  data$samples <- data$samples |>
    factor(
      levels = frequency_rows_subset,
      labels = seq_along(frequency_rows_subset)
    )

  # The text geom is pre-defined here because whether it has a non-`NULL` value
  # depends on a logical argument.
  if (show_text) {
    # Prepare a percent label if the frequency display should be a percentage,
    # at least in part. Otherwise, `label_percent` is `NULL`, so it's ignored.
    # With multiple combined tables, only one set of labels is created.
    label_percent <- switch(
      frequency,
      "percent" = "%",
      "absolute-percent" = data |>
        split(data$samples) |>
        lapply(function(x) {
          freqs <- x$frequency
          percentage <- 100 * round(freqs / sum(freqs), 2)
          paste0(" (", percentage, "%)")
        }) |>
        unlist(use.names = FALSE)
    )

    # Adjust the text offset using the height of the highest bar so that the
    # distance between text and bars is robust to very different values, such as
    # absolute vs. relative values.
    text_offset_adjusted <- text_offset * max(data$frequency)

    # Prepare the text labels above the bars
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

    # Text labels on top of the bars
    geom_text_frequency +

    # Conditionally facet the plot -- needed for `closure_plot_bar_min_max()`
    {
      if (is.null(facet_labels)) {
        # Warn in case the arguments don't quite fit together
        if (!is.null(facet_labels_parens)) {
          cli::cli_warn(
            "`facet_labels_parens` has no effect \
            because `facet_labels` is `NULL`."
          )
        }
        # Evaluate the entire expression to `NULL`
        NULL
      } else {
        # Format facet labels like "Minimal variability (h = 0.68)" with "h" in
        # italics
        facet_labels <- format_equation(
          prefix = facet_labels,
          var_name = facet_labels_parens,
          number = min_max_values,
          mark_decimal = mark_decimal,
          parse_output = FALSE
        )

        # Evaluate the entire expression to a final grid
        ggplot2::facet_grid(
          cols = ggplot2::vars(samples),
          labeller = ggplot2::labeller(
            samples = facet_labels,
            .default = ggplot2::label_parsed
          )
        )
      }
    } +

    # Rest of the plot
    ggplot2::scale_x_continuous(
      breaks = data$value,
      labels = data$value
    ) +
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


#' Visualize CLOSURE data in a histogram
#'
#' @description Call `closure_plot_bar()` to get a barplot of CLOSURE results.
#'
#'   For each scale value, the bars show how often this value appears in the
#'   full list of possible raw data samples found by the CLOSURE algorithm.
#'
#' @param data List returned by [`closure_generate()`] or [`closure_read()`].
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
#' @param text_size Numeric (length 1). Base font size in pt. Default is `12`.
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

# Arguments for this function are generated below the definition
closure_plot_bar <- function() {
  check_closure_generate(data)

  plot_frequency_bar(
    data = data,
    frequency = frequency,
    samples = samples,
    bar_alpha = bar_alpha,
    bar_color = bar_color,
    show_text = show_text,
    text_color = text_color,
    text_size = text_size,
    text_offset = text_offset,
    mark_thousand = mark_thousand,
    mark_decimal = mark_decimal,
    frequency_rows_subset = "all"
  )
}

# Use the arguments of the basic plot function to create a list of arguments for
# this function, but change one default (to use the CLOSURE-specific color) and
# remove two other arguments (to prevent abstraction leaks)
formals(closure_plot_bar) <- plot_frequency_bar |>
  formals() |>
  formals_change_defaults(
    bar_color = "#5D3FD3"
  ) |>
  formals_remove(
    "facet_labels",
    "facet_labels_parens",
    "frequency_rows_subset",
    "min_max_values"
  )


#' Visualize CLOSURE data in an ECDF plot
#'
#' @description Call `closure_plot_ecdf()` to visualize CLOSURE results using
#'   the data's empirical cumulative distribution function (ECDF). This can be
#'   useful to display any variation between CLOSURE samples.
#'
#'   See [`closure_plot_bar()`] for more intuitive visuals.
#'
#' @details This function was inspired by [`rsprite2::plot_distributions()`]
#'   with its option `plot_type = "ecdf"`. However, `plot_distributions()`
#'   invariably shows one line per (randomly drawn) possible dataset, and it
#'   does not support the horns index or other measures of dispersion. Some
#'   further differences exist.
#'
#' @param samples String (length 1). How to map the samples to ECDF lines?
#'   - `"mean_min_max"`, the default, draws three lines: overall mean across all
#'   samples, mean of the samples with the minimum horns index, and mean of the
#'   samples with the maximum horns index.
#'   - `"mean"` draws a single line for the overall mean.
#'   - `"all"` draws a separate line for each sample, colored by its horns
#'   index value. *Note*: This is invalid if `data$results` does not include the
#'   `samples` and `horns` columns. If many samples were found, it can be very
#'   slow or even crash your R session.
#' @param pad String (length 1). How far should the ECDF line(s) stretch?
#'   - `"extend"`, the default, draws the lines to both ends of the y-axis
#'   vertically and slightly beyond that horizontally, as
#'   [`ggplot2::stat_ecdf()`] does by default.
#'   - `"match"` draws them vertically as above, but not horizontally. *Note:*
#'   This is currently invalid in combination with `samples = "all"`.
#'   - `"stop"` does not draw the lines beyond the data points at all.
#' @param legend_title String (length 1). Defaults for the legend title depend
#'   on `samples`:
#'   - With `samples = "mean_min_max"`, the legend title is absent by default
#'   because it can make the legend extend beyond the plot itself. If you do
#'   choose a title, consider `legend_title = "Subset of samples"`.
#'   - With `samples = "mean"`, there is no legend, and hence no title.
#'   - With `samples = "all"`, the title says "Horns index" unless you provide a
#'   different one.
#'
#'   To remove the legend or change its position, use `legend.position` in
#'   [`ggplot2::theme()`].
#' @param line_color_single String (length 1). If `samples` is `"mean"`, this is
#'   the color of the single ECDF line. Default is `"#5D3FD3"`, a purple color.
#' @param line_color_multiple String (length 3). If `samples` is
#'   `"mean_min_max"`, these are the colors of the three ECDF lines. Default is
#'   `"royalblue4"` for the overall mean, `"deeppink"` for the minimum horns
#'   index, and `"darkcyan"` for the maximum horns index; in this order.
#'
#'   If `samples` is `"all"`, the colors for min and max horns index values are
#'   used for the low and high ends of the gradient.
#' @param reference_line_alpha Numeric (length 1). Opacity of the diagonal
#'   reference line. Default is `0.6`.
#' @param mark_decimal String (length 1). Decimal delimiter in the labels.
#'   Default is `"."` (e.g., `"0.15"`).
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
# data <- closure_generate(
#   mean = "3.5",
#   sd = "1.7",
#   n = 70,
#   scale_min = 1,
#   scale_max = 5
# )
# samples <- "mean_min_max"
# pad <- "extend"
# legend_title <- NULL
# line_color_single <- "#5D3FD3"
# line_color_multiple <- c("royalblue4", "deeppink", "darkcyan")
# text_size <- 12
# reference_line_alpha <- 0.6
# mark_decimal <- "."

closure_plot_ecdf <- function(
  data,
  samples = c("mean_min_max", "mean", "all"),
  pad = c("extend", "match", "stop"),
  legend_title = NULL,
  line_color_single = "#5D3FD3",
  line_color_multiple = c("royalblue4", "deeppink", "darkcyan"),
  text_size = 12,
  reference_line_alpha = 0.6,
  mark_decimal = "."
) {
  check_closure_generate(data)

  samples <- rlang::arg_match(samples)
  pad <- rlang::arg_match(pad)

  check_length(line_color_single, 1L)
  check_length(line_color_multiple, 3L)

  # For the reference line and the x-axis
  inputs <- data$inputs
  metrics_main <- data$metrics_main
  values_unique <- inputs$scale_min:inputs$scale_max

  # Horns index values for all three subsets of samples
  h_mean <- data$metrics_horns$mean
  h_min <- data$metrics_horns$min
  h_max <- data$metrics_horns$max

  # Will be changed if `samples` is "mean_min_max"
  scale_legend <- NULL

  # Will be changed if `samples` is "all"
  scale_gradient <- NULL

  if (samples == "all") {
    # Error if the raw data are not available -- visualizing all samples is not
    # possible in this case
    if (is.null(data[["results"]][["sample"]])) {
      cli::cli_abort(
        message = c(
          "Visualizing all samples requires those samples.",
          "x" = "`samples` is \"all\" but the actual samples \
          are not present. Most likely, they were written to disk instead.",
          "i" = "See `closure_read()` for options to read \
          all samples from disk. If successful, this will \
          enable you to visualize each individual sample."
        ),
        call = rlang::caller_env()
      )
    }

    # This error shouldn't occur if the initial checks work
    if (is.null(data[["results"]][["horns"]])) {
      cli::cli_abort("Column `horns` missing.", call = rlang::caller_env())
    }

    # Error if the user tries to match the curves of all samples with the range
    if (pad == "match") {
      cli::cli_abort(
        message = c(
          "In `closure_plot_ecdf()`, matching the y-axis range is \
          currently not supported when visualizing each individual sample.",
          "x" = "You chose `samples = \"all\"`, but also `pad = \"match\"`.",
          "i" = "Use `pad = \"extend\"` or `pad = \"stop\"` instead."
        ),
        call = rlang::caller_env()
      )
    }

    # Zoom in on the detailed `results` -- the key element of `data` needed
    # here. Flatten them into a single integer vector. If all samples should be
    # shown, enable grouping the values by sample using a `sample_id` column.
    data <- tibble::new_tibble(
      x = list(
        value = data$results$sample |>
          unlist(use.names = FALSE),

        horns = data$results$horns |>
          rep(each = inputs$n),

        sample_id = metrics_main$samples_all |>
          seq_len() |>
          rep(each = inputs$n)
      ),
      nrow = metrics_main$values_all
    )

    # Color the lines using a gradient derived from the colors for min and max
    # horns index values
    scale_gradient <- ggplot2::scale_color_gradient(
      low = line_color_multiple[2],
      high = line_color_multiple[3]
    )

    legend_position <- "right"

    # With all samples, there actually is a default legend title
    if (is.null(legend_title)) {
      legend_title <- "Horns index"
    }

    # Prepare the geom-like ggplot2 object that maps the data to the ECDF
    # line(s). Group the atomic integer values by `sample_id`.
    stat_ecdf_line <- ggplot2::stat_ecdf(
      ggplot2::aes(
        group = .data$sample_id,
        color = .data$horns
      ),
      pad = pad %in% c("extend", "match")
    )
  } else {
    data <- data$frequency

    # Frequency-based ECDF plots that do not require individual samples
    if (samples == "mean") {
      data <- data[data$samples == "all", c("samples", "value", "f_absolute")]
      data <- mutate_ecdf(data, pad = pad)

      legend_position <- "none"

      stat_ecdf_line <- ggplot2::geom_step(
        ggplot2::aes(x = value, y = .data$ecdf),
        color = line_color_single,
        direction = "hv"
      )
    } else if (samples == "mean_min_max") {
      data <- data[c("samples", "value", "f_absolute")]

      # Necessary (and current) order of the unique values in `data$samples`
      group_order <- c("all", "horns_min", "horns_max")

      # Calculate ECDF directly from frequency table per group. As this reorders
      # the groups implicitly, bring the groups into the same order as they were
      # before splitting, then recombine them into a single tibble.
      data <- data |>
        split(data$samples) |>
        lapply(mutate_ecdf, pad = pad) |>
        (function(x) x[group_order])() |>
        do.call(what = rbind)

      # Apply custom legend labels
      data$samples <- data$samples |>
        factor(levels = group_order)

      # Prepare "stairstep" lines that go horizontal first, vertical second
      # ("hv"). The seemingly obvious alternative, `stat_ecdf()`, would use the
      # raw data even though the frequencies are already known.
      stat_ecdf_line <- ggplot2::geom_step(
        ggplot2::aes(x = value, y = ecdf, color = samples, group = samples),
        direction = "hv"
      )

      # The legend is only shown in this three-line case, so its labels are only
      # prepared here
      labels_legend <- format_equation(
        prefix = c("All samples", "Min variance", "Max variance"),
        var_name = "h",
        number = c(h_mean, h_min, h_max),
        mark_decimal = mark_decimal,
        parse_output = TRUE
      )

      legend_position <- "bottom"

      # Format the legend
      scale_legend <- ggplot2::scale_color_manual(
        values = line_color_multiple,
        labels = labels_legend
      )
    } else {
      cli::cli_abort("Internal error: unhandled `samples` type.")
    }
  }

  # Construct the ECDF plot
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +

    # ECDF line:
    stat_ecdf_line +

    # Dashed diagonal reference line:
    ggplot2::annotate(
      geom = "segment",
      linetype = 2,
      alpha = reference_line_alpha,
      x = values_unique[1L],
      xend = values_unique[length(values_unique)],
      y = 0,
      yend = 1
    ) +

    # Rest of the plot:
    ggplot2::labs(
      x = "Scale value",
      y = "Cumulative share",
      color = legend_title
    ) +
    ggplot2::scale_x_continuous(
      breaks = values_unique,
      labels = values_unique
    ) +
    scale_legend +
    scale_gradient +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(legend.position = legend_position)
}


# Internal helpers --------------------------------------------------------

#' Add columns for cumulative frequency and ECDF
#'
#' Internal helper that calculates the cumulative frequencies and the ECDF from
#' the frequency table used inside of `closure_plot_ecdf()`.
#'
#' @param data Data frame that contains these columns (and no others):
#'   `"samples"`, `"value"`, `"f_absolute"`.
#' @param pad Logical. Should the groups be padded with extra rows that have
#'   zeros for frequencies? In an ECDF ggplot created manually using
#'   `ggplot2::geom_step()`, this will have the same effect as `pad` in
#'   `ggplot2::stat_ecdf()`.
#'
#' @returns Data frame with the input columns plus `"ecdf"`.
#'
#' @noRd
mutate_ecdf <- function(data, pad) {
  cumulative_freq <- cumsum(data$f_absolute)

  # Normalize to get cumulative probabilities (ECDF values)
  total_freq <- sum(data$f_absolute)
  data$ecdf <- cumulative_freq / total_freq

  if (pad == "stop") {
    return(data)
  }

  # `pad = "extend"` behaves just like `pad = TRUE` in `ggplot2::stat_ecdf()`.
  # By contrast, `pad = "match"` only extends the horizontal lines to an
  # imperceptible degree, which is enough to clearly display the vertical
  # lines at the scale limits. With larger values, a horizontal extension of the
  # lines might be visible.
  value_pad <- switch(pad, "extend" = 0.5, "match" = 0.002)

  # Add point at the beginning
  value_first <- data$value[1]
  pad_start <- list(
    samples = data$samples[1],
    value = value_first - value_pad,
    f_absolute = 0,
    ecdf = 0
  )

  # Add point at the end (extend last value, ecdf = 1)
  value_last <- data$value[length(data$value)]
  pad_end <- list(
    samples = data$samples[1],
    value = value_last + value_pad,
    f_absolute = 0,
    ecdf = 1
  )

  # Pad the data with the extra rows
  rbind(pad_start, data, pad_end)
}


#' Format equation labels for ggplot2
#'
#' Internal helper to make ggplot2 render a label like "All samples (h = 0.34)"
#' with "h" in italics. Vectorized over all arguments. Returns a string vector
#' by default, but can optionally be parsed into an expression vector.
#'
#' @param prefix String. Will go before the parentheses, e.g., `"All samples"`.
#' @param var_name String. LHS of the equation. This part will be in italics;
#'   e.g., `"h"`.
#' @param number Numeric. RHS of the equation. Will be rounded to 2 decimal
#'   places, e.g., `0.34`.
#' @param mark_decimal String. Decimal sign to use in `number` , e.g., `"."`.
#' @param parse_output Logical. Should the output be parsed as an expression?
#'   Default is `FALSE`.
#'
#' @returns Vector of strings (or, with `parse_output = TRUE`, expressions) to
#'   be used as ggplot2 labels. They are formatted in plotmath, a somewhat
#'   obscure syntax for use in R graphics. Although plotmath is difficult and
#'   `ggtext::element_markdown()` offers an easy alternative, this function does
#'   it the hard way to avoid a dependency.
#'
#' @noRd
format_equation <- function(
  prefix,
  var_name,
  number,
  mark_decimal,
  parse_output = FALSE
) {
  # Spaces are represented as tildes
  prefix <- gsub(" ", "~", prefix)

  # Format the number to two decimal places with `mark_decimal` as the separator
  number <- scales::label_number(
    accuracy = 0.01,
    decimal.mark = mark_decimal
  )(number)

  # Assemble a string that will make ggplot2 render `var_name` in italics
  out <- paste0(prefix, "~(italic(", var_name, ")~`=`~", number, ")")

  # Needed for `ggplot2::labeller()`, though not for scale functions
  names(out) <- seq_along(out)

  # Convert the string into an expression. This is required by ggplot2 in some
  # places like labels in scales.
  if (parse_output) {
    parse(text = out)
  } else {
    out
  }
}
