# # For interactive testing:
# data <- closure_generate(
#   mean = "3.5",
#   sd = "1.7",
#   n = 70,
#   scale_min = 1,
#   scale_max = 5
# )
# format <- "absolute_percent"
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
# facet_labels <- c("Minimal variance", "Maximal variance")
# facet_labels_parens <- "h"
# min_max_values <- c(0.41, 0.42)

plot_frequency_bar <- function(
  data,
  technique,
  format = c(
    "percent",
    "absolute_percent",
    "absolute",
    "relative"
  ),
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
  check_generator_output(data, technique)

  # With a demo plot, construct a frequency table like those from `*_generate()`
  # from the input `data`, which is simply a vector of frequencies.
  if (technique == "DEMO") {
    check_type(data, c("double", "integer"))

    data <- tibble::new_tibble(
      list(
        samples = rep("all", length(data)),
        value = seq_along(data),
        # `f_average` is not supported
        f_absolute = data,
        f_relative = data / sum(data)
      ),
      nrow = length(data)
    )

    # As demo plots don't have a `samples` argument but the remainder of the
    # current function expects one, it is substituted here
    samples <- "all"
  } else {
    # Zoom in on the frequency table -- the only element of `data` needed here.
    # Filter its rows to only keep those with a specific subset of samples, such
    # as "horns_min" and "horns_max", or "all" for all samples taken together.
    data <- data$frequency
    data <- data[data$samples %in% frequency_rows_subset, ]
  }

  # Check inputs here because demo plots don't have `samples`
  format <- rlang::arg_match(format)
  samples <- rlang::arg_match(samples)

  # How many subsets of samples are there? E.g., 2 with min-max grouping
  n_samples_groups <- length(unique(data$samples))

  # In terms of absolute frequencies, the user may choose to show the average
  # number of observations per bin instead of the full count. If so, replace the
  # full absolute values by the average values, and prepare a label to signpost
  # the average inside of the plot.
  if (samples == "mean") {
    data$f_absolute <- data$f_average
    label_avg_all <- "avg. sample, *n* = "
    label_values <- " "
  } else if (samples == "all" && n_samples_groups == 1) {
    label_avg_all <- "all "
    label_values <- " values "
  } else if (samples == "all") {
    label_avg_all <- "the subset of values"
    label_values <- ""
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
    accuracy = if (format == "relative") {
      0.01
    } else if (samples == "mean" || format == "percent") {
      0.1
    } else {
      1
    },
    big.mark = mark_thousand,
    decimal.mark = mark_decimal
  )

  # Next, specify the y-axis label by format type, then remove the column
  # that represents the main non-chosen type of format: absolute or relative
  if (format %in% c("absolute", "absolute_percent")) {
    # In case the average sample is displayed (either of the whole results set
    # or per facet / subset), format the number of values found: no decimal
    # places but a comma or similar to separate levels of thousand every three
    # digits. This value is either the reported `n` -- i.e., inter alia, the
    # size of the average sample -- or the total number of values found by the
    # CLOSURE-type technique.
    label_mean_count <- if (samples == "mean") {
      data$f_absolute |>
        call_on(function(x) sum(x) / n_samples_groups) |>
        call_on(scales::label_number(
          accuracy = 1,
          big.mark = mark_thousand
        ))
    } else {
      ""
    }

    label_y_axis <- paste0(
      "Count in ",
      label_avg_all,
      label_mean_count,
      label_values,
      switch(format, "absolute_percent" = " (%)")
    )
    data$f_relative <- NULL
  } else if (format == "relative") {
    label_y_axis <- "Relative frequency"
    data$f_absolute <- NULL
  } else if (format == "percent") {
    label_y_axis <- "Percentage of all values"
    data$f_relative <- round(100 * data$f_relative, 2)
    data$f_absolute <- NULL
  } else {
    cli::cli_abort("Internal error: unhandled `format` type.")
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
      format,
      "percent" = "%",
      "absolute_percent" = data |>
        split(data$samples) |>
        lapply(function(x) {
          freqs <- x$frequency
          percentage <- round(100 * freqs / sum(freqs), 1)
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
      if (!show_text) {
        NULL
      } else if (is.null(facet_labels) && technique != "DEMO") {
        # Warn in case the arguments don't quite fit together
        if (!is.null(facet_labels_parens)) {
          cli::cli_warn(
            "`facet_labels_parens` has no effect \
            because `facet_labels` is `NULL`."
          )
        }
        # Evaluate the entire expression to `NULL`
        NULL
      } else if (technique == "DEMO") {
        # Compute the horns index for the example frequency distribution, then
        # add the uniform horns index based only on the number of scale points.
        label_h <- data$frequency |>
          horns(1, nrow(data)) |>
          c(horns_uniform(1, nrow(data))) |>
          call_on(scales::label_number(
            accuracy = 0.01,
            decimal.mark = mark_decimal
          ))

        # Prepare a pseudo-"facet" label for a single demo plot. It will say,
        # e.g., "h = 0.32, h_u = 0.50" for the horns index and the uniform one.
        # There is only one set of such labels because example data are not
        # min-max grouped. Naming the label is needed to display it via ggplot2.
        label_h <- paste0(
          c("*h* = ", "*h*<sub>u</sub> = "),
          label_h,
          collapse = ", "
        )
        names(label_h) <- "1"

        ggplot2::facet_grid(
          cols = ggplot2::vars(samples),
          labeller = ggplot2::labeller(samples = label_h)
        )
      } else {
        # Format facet labels like "Minimal variance (h = 0.68)" with "h" in
        # italics. The labeller expects a named vector where names match the
        # factor levels (which are "1", "2", etc.).
        facet_labels <- format_equation(
          prefix = facet_labels,
          number = min_max_values,
          mark_decimal = mark_decimal,
          var_name = facet_labels_parens
        )

        names(facet_labels) <- as.character(seq_along(facet_labels))

        # Evaluate the entire expression to a final grid
        ggplot2::facet_grid(
          cols = ggplot2::vars(samples),
          labeller = ggplot2::labeller(samples = facet_labels)
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
      # Enable markdown formatting in x and y axis labels (e.g., "h" in italics)
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown(),
      # Enable markdown formatting in facet labels and control their size
      strip.text = ggtext::element_markdown(size = text_size),
      # Leave out vertical grid lines because the x-axis is categorical
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}
