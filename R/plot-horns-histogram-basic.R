# Internal basis of the `*_plot_horns_*()` functions
plot_horns_frequency <- function(
  data,
  type,
  technique,
  bar_color,
  alpha = 0.75,
  binwidth = 0.01,
  show_labels = c(
    "all",
    "min_max",
    "min_max_uniform",
    "min_max_bounds",
    "uniform",
    "uniform_bounds",
    "bounds",
    "none"
  ),
  line_color_min_max = "red",
  line_color_reference = "grey20",
  text_limits = c(0.12, 0.88),
  text_size = 12,
  mark_thousand = ",",
  mark_decimal = "."
) {
  check_generator_output(data, technique)

  check_length(text_limits, 2L)

  show_labels <- arg_match_in_export(show_labels)

  # Key statistics about the horns index distribution. Lines and labels will be
  # placed at or around these three points (min and max label placement varies).
  h_min <- data@metrics_horns$min
  h_max <- data@metrics_horns$max
  h_uniform <- data@metrics_horns$uniform

  # If the minimum horns value is too close to 0 for the min label to fit on its
  # left, or the maximum value is too close to 1 for the max label to fit on its
  # right, check the distribution shape to decide where to place labels. Get the
  # median of the distribution to understand where most data lies.
  h_median <- data@metrics_horns$median

  n_samples_all <- data@metrics_main$samples_all

  # `directory` is only present in from-disk classes, not in-memory results
  path <- if (
    S7::S7_inherits(data, ClosureResultFromDisk) ||
      S7::S7_inherits(data, SpriteResultFromDisk)
  ) {
    data@directory$path
  } else {
    NULL
  }

  # Reduce the input to a tibble that only includes the horns values.
  # Classes without a `results` property (stats-only) have no horns column.
  has_results <- !S7::S7_inherits(data, ClosureResultStatsOnly) &&
    !S7::S7_inherits(data, SpriteResultStatsOnly)
  data <- if (has_results) data@results["horns"] else NULL

  # Error if no "horns" column is present
  if (is.null(data)) {
    fn_name <- caller_fn_name()

    msg_reader_call <- paste0(
      tolower(technique),
      "_read(\"stats_and_horns\") |>"
    )

    msg_read <- if (is.null(path)) {
      NULL
    } else {
      c(
        "i" = "To read these values from disk and plot them, run:",
        "\n",
        "{.emph path <- \"{path}\"}",
        "\n",
        "{.emph path |>}\n",
        " " = "{.emph {msg_reader_call}}\n",
        " " = "{.emph {fn_name}()}"
      )
    }

    abort_in_export(
      "Need \"horns\" column in `results` tibble.",
      "x" = "This function is all about visualizing the distribution of
      horns index values.",
      msg_read
    )
  }

  # Position labels: min label to the left, max label to the right. Add a small
  # horizontal offset from the line.
  label_offset <- 0.02

  position_x_min <- max(0, h_min - label_offset)
  position_x_max <- min(1, h_max + label_offset)

  # Left-align min label, right-align max label
  hjust_min <- 1
  hjust_max <- 0

  vjust_min <- 2
  vjust_max <- 2

  # Estimate horizontal space needed for a label (14 or fewer characters). As a
  # rough estimate, 14 characters take about 0.14-0.16 of the plot width.
  label_width <- 0.15
  lines_too_close <- h_max - h_min < label_width

  # Check if min and max lines are very close AND distribution is at one extreme
  if (lines_too_close && h_min < text_limits[1]) {
    # Lines too close and both near left edge: stack both labels on the right
    position_x_min <- position_x_max
    position_x_max <- position_x_max
    hjust_min <- 0
    hjust_max <- 0
    vjust_min <- 2
    vjust_max <- 3.5 # Max label below min label
  } else if (lines_too_close && h_max > text_limits[2]) {
    # Lines too close and both near right edge: stack both labels on the left
    position_x_min <- position_x_min
    position_x_max <- position_x_min
    hjust_min <- 1
    hjust_max <- 1
    vjust_min <- 2
    vjust_max <- 3.5 # Max label below min label
  } else if (h_min < text_limits[1]) {
    # Min label doesn't fit on the left side
    if (h_median > 0.5) {
      # Distribution concentrated on left, stack labels on the right
      position_x_min <- position_x_max
      hjust_min <- hjust_max
      vjust_max <- 3.5
    } else {
      # Distribution concentrated on right, space on left - keep min label alone
      # on left at edge, but closer to the line
      position_x_min <- h_min
      # Left-align so label extends to the right
      hjust_min <- 0
      vjust_min <- 2
    }
  } else if (h_max > text_limits[2]) {
    # Max label doesn't fit on the right side
    if (h_median < 0.5) {
      # Distribution concentrated on right, stack labels on the left
      position_x_max <- position_x_min
      hjust_max <- hjust_min
      vjust_max <- 3.5
    } else {
      # Distribution concentrated on left, space on right - keep max label alone
      # on right at edge, but closer to the line
      position_x_max <- h_max
      # Right-align so label extends to the left
      hjust_max <- 1.2
      vjust_max <- 2
    }
  }

  # Adjust uniform label position to avoid overlap with min/max labels
  # Uniform label is centered, so it extends about 0.075 on each side
  label_half_width <- 0.075
  position_x_uniform <- h_uniform
  hjust_uniform <- 0.5

  # Check if uniform label overlaps with min label (which is at the top)
  # We need to check horizontal overlap based on the actual label positions
  uniform_overlaps_min <- abs(position_x_uniform - position_x_min) < label_width
  uniform_overlaps_max <- abs(position_x_uniform - position_x_max) < label_width

  if (uniform_overlaps_min && uniform_overlaps_max) {
    # Overlaps with both: move to the side with more space
    if (h_median < 0.5) {
      # Distribution on left, space on right - move uniform label right
      position_x_uniform <- min(1, h_uniform + label_offset)
      hjust_uniform <- 0 # Left-align so it extends to the right
      # Don't shift if it would go out of bounds
      if (position_x_uniform + label_half_width > 1) {
        position_x_uniform <- h_uniform
        hjust_uniform <- 0.5
      }
    } else {
      # Distribution on right, space on left - move uniform label left
      position_x_uniform <- max(0, h_uniform - label_offset)
      hjust_uniform <- 1 # Right-align so it extends to the left
      # Don't shift if it would go out of bounds
      if (position_x_uniform - label_half_width < 0) {
        position_x_uniform <- h_uniform
        hjust_uniform <- 0.5
      }
    }
  } else if (uniform_overlaps_min && !uniform_overlaps_max) {
    # Overlaps with min label only: shift uniform label left
    position_x_uniform <- max(0, h_uniform - label_offset)
    # Right-align so it extends to the left
    hjust_uniform <- 1
    # Don't shift if it would go out of bounds
    if (position_x_uniform - label_half_width < 0) {
      position_x_uniform <- h_uniform
      hjust_uniform <- 0.5
    }
  } else if (uniform_overlaps_max && !uniform_overlaps_min) {
    # Overlaps with max label only: shift uniform label right
    position_x_uniform <- min(1, h_uniform + label_offset)
    # Left-align so it extends to the right
    hjust_uniform <- 0
    # Don't shift if it would go out of bounds
    if (position_x_uniform + label_half_width > 1) {
      position_x_uniform <- h_uniform
      hjust_uniform <- 0.5
    }
  }
  # If overlaps with neither, keep centered at h_uniform

  # String with labels such as "Min (h = 0.68)" and "Max (h = 0.75)"; where "h"
  # is in italics. Using ggtext markdown formatting.
  label_min_max <- format_equation(
    prefix = c("Min<br>", "Max<br>"),
    number = c(h_min, h_max),
    mark_decimal = mark_decimal
  )

  label_uniform <- format_equation(
    prefix = "Uniform<br>",
    number = h_uniform,
    mark_decimal = mark_decimal,
    subscript = "u"
  )

  # Construct the plot
  ggplot2::ggplot(data) +
    ggplot2::geom_histogram(
      ggplot2::aes(x = .data$horns),
      alpha = alpha,
      fill = bar_color,
      binwidth = binwidth
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      oob = function(x, limits) x
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = mark_thousand)
    ) +

    # Min and max reference lines
    ggplot2::geom_vline(
      xintercept = c(h_min, h_max),
      linetype = 2,
      alpha = 0.75,
      color = line_color_min_max,
      linewidth = 0.75
    ) +

    # Reference lines for perfect unimodality / invariance (h = 0), perfect
    # uniformity (h_u), and perfect bimodality / polarization (h = 1)
    ggplot2::geom_vline(
      xintercept = c(0, h_uniform, 1),
      # Uniform line is dotted and slightly wider
      linetype = c(1, 3, 1),
      linewidth = c(0.5, 0.75, 0.5),
      color = line_color_reference,
    ) +

    # Text label for min and max lines -- optional (1 / 3)
    {
      if (show_labels == "all" || grepl("min_max", show_labels)) {
        ggtext::geom_richtext(
          data = tibble::new_tibble(
            list(
              x = c(position_x_min, position_x_max),
              y = Inf,
              label = label_min_max,
              vjust = c(vjust_min, vjust_max),
              hjust = c(hjust_min, hjust_max)
            ),
            nrow = 2L
          ),
          ggplot2::aes(
            x = .data$x,
            y = .data$y,
            label = .data$label,
            vjust = .data$vjust,
            hjust = .data$hjust
          ),
          color = line_color_min_max,
          fill = "white",
          # Determine padding for top, right, bottom, left; in "trouble" order
          label.padding = grid::unit(c(0.4, 0.4, 0.4, 0.4), "lines")
        )
      } else {
        NULL
      }
    } +

    # Text label for uniform line -- optional (2 / 3)
    {
      if (show_labels == "all" || grepl("uniform", show_labels)) {
        ggtext::geom_richtext(
          data = tibble::new_tibble(
            list(
              x = position_x_uniform,
              y = -Inf,
              label = label_uniform
            ),
            nrow = 1L
          ),
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
          vjust = -3,
          hjust = hjust_uniform,
          color = line_color_reference,
          fill = "white",
          # Center the label within the box by leaving less space at the bottom
          # to cancel out the distorting effect of the subscript
          label.padding = grid::unit(c(0.5, 0.5, 0.25, 0.5), "lines")
        )
      } else {
        NULL
      }
    } +

    # Text labels for h = 0 and h = 1 -- optional (3 / 3)
    {
      if (show_labels == "all" || grepl("bounds", show_labels)) {
        ggtext::geom_richtext(
          data = tibble::new_tibble(
            list(
              x = c(0, 1),
              y = c(-Inf, -Inf),
              label = c(
                "Perfect unimodal,<br>all values equal<br>(*h* = 0)",
                "Perfect bimodal,<br>polarized split<br>(*h* = 1)"
              )
            ),
            nrow = 2L
          ),
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
          vjust = -0.75,
          hjust = c(-0.05, 1.05),
          color = line_color_reference,
          fill = "white",
          label.padding = grid::unit(c(0.4, 0.4, 0.4, 0.4), "lines")
        )
      } else {
        NULL
      }
    } +

    # Rest of the plot
    ggplot2::labs(
      x = "Horns index (*h*)",
      # Special y-axis label if there is exactly 1 h value -- otherwise, a label
      # that includes the number of values is assembled
      y = if (n_samples_all == 1) {
        "\"Count\" of the single *h* value"
      } else {
        paste(
          "Count in all",
          n_samples_all |>
            call_on(scales::label_number(big.mark = mark_thousand)),
          "*h* values"
        )
      }
    ) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      # Enable markdown formatting in axis labels to display "h" in italics
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown(),
    )
}
