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
  min_max = c("both", "min", "max"),
  format = c(
    "percent",
    "absolute_percent",
    "absolute",
    "relative"
  ),
  samples = c("mean", "all"),
  overlay = c("all_avg", "none", "interval", "dots"),
  facet_labels = c("Minimal variance", "Maximal variance"),
  facet_labels_parens = "h",
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

  data_overlay <- NULL

  # With a demo plot, construct a frequency table like those from `*_generate()`
  # from the input `data`, which is simply a vector of frequencies.
  if (technique == "DEMO") {
    check_type(data, c("double", "integer"))
    check_frequency_vector(data)

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
    frequency_rows_subset <- "all"
  } else {
    min_max <- arg_match_in_export(min_max)

    check_length(facet_labels, 2L, allow_null = TRUE)
    check_length(facet_labels_parens, 1L, allow_null = TRUE)

    # Enable plots without facet labels
    if (is.null(facet_labels)) {
      facet_labels <- rep("\"\"", 2)
      facet_labels_parens <- NULL
    }

    # Derive internal values from `min_max`
    min_max_values <- c(data$metrics_horns$min, data$metrics_horns$max)

    frequency_rows_subset <- switch(
      min_max,
      "both" = c("horns_min", "horns_max"),
      "min" = "horns_min",
      "max" = "horns_max"
    )

    facet_labels <- switch(
      min_max,
      "both" = facet_labels,
      "min" = facet_labels[1L],
      "max" = facet_labels[2L]
    )

    # Save components needed for overlay modes before `data` is overwritten
    data_frequency_all <- data$frequency[data$frequency$samples == "all", ]
    data_results <- data$results
    data_inputs <- data$inputs

    # Zoom in on the frequency table -- the only element of `data` needed here.
    # Filter its rows to only keep those with a specific subset of samples, such
    # as "horns_min" and "horns_max".
    data <- data$frequency
    data <- data[data$samples %in% frequency_rows_subset, ]
  }

  # Check inputs here because demo plots don't have `samples`
  format <- arg_match_in_export(format)
  samples <- arg_match_in_export(samples)
  overlay <- arg_match_in_export(overlay)

  need_all_samples <- overlay %in% c("dots", "interval")

  if (overlay != "none") {
    # Need the ggdist package to visualize the distribution of all samples
    if (need_all_samples) {
      rlang::check_installed(
        "ggdist",
        reason = paste0("for overlay = \"", overlay, "\".")
      )
    }

    # Two possible errors -- one by the developer, one by the user
    if (technique == "DEMO") {
      cli::cli_abort(c(
        "Internal error: DEMO plots need `overlay == {.val none}`.",
        "x" = "DEMO plot with `overlay == {.val {overlay}}`."
      ))
    } else if (samples != "mean") {
      abort_in_export(
        "Overlay mode {.val {overlay}} requires `samples` to be {.val mean}.",
        "x" = "`samples` was specified as {.val {samples}}."
      )
    }
  }

  # Build overlay data. All overlay modes require `samples == "mean"` so that
  # both the main bars and the overlay show per-sample averages and are on the
  # same scale.
  if (overlay == "none") {
    data_overlay <- NULL
  } else if (overlay == "all_avg") {
    # Gray background bars: the mean frequency across all CLOSURE samples
    data_overlay <- data_frequency_all
    data_overlay$f_absolute <- data_overlay$f_average
    data_overlay$f_average <- NULL
  } else if (need_all_samples) {
    # Build a per-sample frequency table with one row per CLOSURE sample per
    # scale value. Requires individual sample vectors.
    if (!"sample" %in% names(data_results)) {
      lowtech <- tolower(technique)
      fn_name_gen <- paste0(lowtech, "_generate")
      fn_name_read <- paste0(lowtech, "_read")
      abort_in_export(
        "Overlay mode {.val {overlay}} requires individual sample data.",
        "i" = "Newly generate with `include = \"all\"` in `{fn_name_gen}()`
        or newly read with `{fn_name_read}()`."
      )
    }

    scale_vals <- seq(data_inputs$scale_min, data_inputs$scale_max)
    n_scale_vals <- length(scale_vals)
    scale_min_val <- data_inputs$scale_min

    data_overlay <- do.call(
      what = rbind,
      args = lapply(data_results$sample, \(samp) {
        counts <- tabulate(samp - scale_min_val + 1L, nbins = n_scale_vals)
        freq <- switch(
          format,
          "relative" = counts / sum(counts),
          "percent" = 100 * counts / sum(counts),
          as.double(counts)
        )

        if (length(scale_vals) != length(freq)) {
          cli::cli_abort(c(
            "Internal error: incongruent lengths.",
            "x" = "`scale_vals` is length {length(scale_vals)}",
            "x" = "`freq` is length {length(freq)}"
          ))
        }

        # tibble::tibble(value = scale_vals, frequency = freq)
        tibble::new_tibble(
          list(value = scale_vals, frequency = freq),
          nrow = length(freq)
        )
      })
    )
  } else {
    cli::cli_abort("Internal error: invalid \"overlay\" variant \"{overlay}\".")
  }

  print(data_overlay)

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

  # Apply format transformation to the all_avg overlay; interval/dots build
  # their overlay with the format already applied
  if (!is.null(data_overlay) && overlay == "all_avg") {
    if (format %in% c("absolute", "absolute_percent")) {
      data_overlay$f_relative <- NULL
    } else if (format == "relative") {
      data_overlay$f_absolute <- NULL
    } else if (format == "percent") {
      data_overlay$f_relative <- round(100 * data_overlay$f_relative, 2)
      data_overlay$f_absolute <- NULL
    }
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

  # Rename overlay columns consistently, then duplicate it for each facet so
  # it appears in every panel
  if (!is.null(data_overlay)) {
    names(data_overlay)[
      !(names(data_overlay) %in% c("samples", "group_frequency_table"))
    ] <- c(
      "value",
      "frequency"
    )
    data_overlay <- do.call(
      rbind,
      lapply(seq_along(frequency_rows_subset), function(i) {
        d <- data_overlay
        d$samples <- factor(i, levels = seq_along(frequency_rows_subset))
        d
      })
    )
  }

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
          percentage <- 100 * freqs / sum(freqs)
          percentage <- round(percentage, 1)
          paste0(" (", percentage, "%)")
        }) |>
        unlist(use.names = FALSE)
    )

    # Adjust the text offset using the height of the highest bar so that the
    # distance between text and bars is robust to very different values, such as
    # absolute vs. relative values.
    text_offset_adjusted <- text_offset * max(data$frequency)

    # Prepare the text labels above the bars
    geom_text_frequency <- ggplot2::geom_label(
      ggplot2::aes(
        y = frequency + text_offset_adjusted,
        label = paste0(format_number_label(frequency), label_percent)
      ),
      color = text_color,
      fill = "white",
      size = text_size / 3
    )
  } else {
    geom_text_frequency <- NULL
  }

  # For the dots overlay: precompute the above/below split here so both the
  # before-bars and after-bars positions in the ggplot chain can reference them
  if (overlay == "dots" && !is.null(data_overlay)) {
    key_overlay <- paste(data_overlay$value, data_overlay$samples)
    key_bars <- paste(data$value, data$samples)
    bar_ht <- data$frequency[match(key_overlay, key_bars)]
    data_overlay$frequency <- pmax(data_overlay$frequency, 0)
    data_dots_above <- data_overlay[data_overlay$frequency > bar_ht, ]
    data_dots_below <- data_overlay[data_overlay$frequency <= bar_ht, ]
  } else {
    data_dots_above <- NULL
    data_dots_below <- NULL
  }

  # Construct the bar plot
  ggplot2::ggplot(data, ggplot2::aes(x = value, y = frequency)) +
    # Overlay geom: visualizes the frequency distribution across all CLOSURE
    # samples; rendered first so the main bars appear on top
    {
      if (is.null(data_overlay)) {
        NULL
      } else if (overlay == "all_avg") {
        ggplot2::geom_col(
          data = data_overlay,
          alpha = 0.3,
          fill = "gray50"
        )
      } else if (overlay == "interval") {
        ggdist::stat_interval(
          data = data_overlay,
          ggplot2::aes(x = value, y = frequency),
          .width = c(0.5, 0.8, 0.95),
          alpha = 0.5,
          linewidth = 8
        )
      } else if (overlay == "dots") {
        # Dots must render after the main bars so below-bar white dots appear
        # on the bar surface rather than underneath it; see separate block below
        NULL
      }
    } +
    ggplot2::geom_col(alpha = bar_alpha, fill = bar_color) +

    # Dots overlay: rendered after the bars so below-bar white dots appear
    # as circles on the bar surface, above-bar dots appear above the bar
    {
      if (overlay == "dots" && !is.null(data_overlay)) {
        list(
          if (nrow(data_dots_below) > 0L) {
            ggdist::stat_dots(
              data = data_dots_below,
              ggplot2::aes(x = value, y = frequency),
              fill = "white",
              colour = "white",
              side = "both",
              alpha = 0.5
            )
          },
          if (nrow(data_dots_above) > 0L) {
            ggdist::stat_dots(
              data = data_dots_above,
              ggplot2::aes(x = value, y = frequency),
              fill = bar_color,
              colour = bar_color,
              side = "both",
              alpha = 0.5
            )
          }
        )
      }
    } +

    # Text labels on top of the bars
    geom_text_frequency +

    # Conditionally facet the plot -- needed for `closure_plot_bar()`
    {
      if (!show_text) {
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
