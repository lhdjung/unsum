#' Horns index for each CLOSURE sample
#'
#' @description Following up on [`closure_generate()`], you can call
#'   `closure_horns_analyze()` to compute the horns index for each individual
#'   sample and compute summary statistics on the distribution of these indices.
#'   See [`horns()`] for the metric itself.
#'
#'   This adds more detail to the `"horns"` and `"horns_uniform"` columns in the
#'   output of `closure_generate()`, where `"horns"` is the overall mean of the
#'   per-sample indices found here.
#'
#'   `closure_horns_plot()` draws a quick barplot to reveal the pattern of horns
#'   values.
#'
#' @param data For `closure_horns_analyze()`, a list returned by
#'   `closure_generate()`. For `closure_horns_plot()`, a list returned by
#'   `closure_horns_analyze()`.
#' @inheritParams closure_plot_bar
#'
#' @details The `"mad"` column overrides a default of `stats::mad()`: adjusting
#'   the result via multiplication by a constant (about 1.48). This assumes a
#'   normal distribution, which generally does not seem to be the case with
#'   horns index values. Here, the constant is set to 1.
#'
#' @return `closure_horns_analyze()` returns a named list of two tibbles (data
#'   frames):
#'   - **horns_metrics**: Summary statistics of the distribution of horns index
#'   values:
#'     - `mean`, `uniform`: same as `horns` and `horns_uniform` from
#'   `closure_generate()`'s output.
#'     - `sd`: double. Standard deviation.
#'     - `cv`: double. Coefficient of variation, i.e., `sd / mean`.
#'     - `mad`: double. Median absolute deviation; see [`stats::mad()`].
#'     - `min`, `median`, `max`: double. Minimum, median, and maximum horns
#'   index.
#'     - `range`: double. Equal to `max - min`.
#'   - **horns_results**:
#'     - `id`: integer. Uniquely identifies each horns index, just like their
#'   corresponding samples in `closure_generate()`.
#'     - `horns`: double. Horns index for each individual sample.
#'
#'   `closure_horns_plot()` returns a ggplot object.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' data <- closure_generate(
#'   mean = "2.9",
#'   sd = "0.5",
#'   n = 70,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' data_horns <- closure_horns_analyze(data)
#' data_horns
#'
#' closure_horns_plot(data_horns)

closure_horns_analyze <- function(data) {
  check_closure_generate(data)

  n_samples_all <- data$metrics$samples_all

  scale_min <- data$inputs$scale_min
  scale_max <- data$inputs$scale_max
  scale_length <- length(scale_min:scale_max)

  horns_values <- numeric(n_samples_all)

  # Calculate the horns index of every single sample
  for (i in seq_len(n_samples_all)) {
    f_absolute <- data$results$sample[[i]] |>
      unlist(use.names = FALSE) |>
      tabulate(nbins = scale_length)

    horns_values[i] <- horns(
      freqs = f_absolute,
      scale_min = scale_min,
      scale_max = scale_max
    )
  }

  mean <- data$metrics$horns
  sd <- sd(horns_values)
  min <- min(horns_values)
  max <- max(horns_values)

  out <- list(
    placeholder_name = data$inputs,
    horns_metrics = tibble::new_tibble(
      x = list(
        mean = mean,
        uniform = data$metrics$horns_uniform,
        sd = sd,
        cv = sd / mean,
        mad = stats::mad(horns_values, constant = 1),
        min = min,
        median = stats::median(horns_values),
        max = max,
        range = max - min
      ),
      nrow = 1L
    ),
    horns_results = tibble::new_tibble(
      x = list(
        id = seq_len(n_samples_all),
        horns = horns_values
      ),
      nrow = n_samples_all
    )
  )

  # Anticipating a future parametrization of the technique's name
  names_new <- names(out)
  names_new <- c(
    "closure_generate_inputs",
    names_new[2L:length(names_new)]
  )

  names(out) <- names_new

  out
}


#' @rdname closure_horns_analyze
#' @export

closure_horns_plot <- function(
  data,
  bar_alpha = 0.75,
  bar_color = "#5D3FD3",
  text_size = 12
) {

  msg_analyze_output <- "Need output of `closure_horns_analyze()`."

  check_component_tibble(
    x = data$closure_generate_inputs,
    name = "closure_generate_inputs",
    dims = c(1L, 7L),
    col_names_types = list(
      "mean" = c("character"),
      "sd" = c("character"),
      "n" = c("integer", "double"),
      "scale_min" = c("integer", "double"),
      "scale_max" = c("integer", "double"),
      "rounding" = c("character"),
      "threshold" = c("integer", "double")
    ),
    msg_main = msg_analyze_output
  )

  check_component_tibble(
    x = data$horns_metrics,
    name = "horns_metrics",
    dims = c(1L, 9L),
    col_names_types = list(
      "mean" = c("integer", "double"),
      "uniform" = c("integer", "double"),
      "sd" = c("integer", "double"),
      "cv" = c("integer", "double"),
      "mad" = c("integer", "double"),
      "min" = c("integer", "double"),
      "median" = c("integer", "double"),
      "max" = c("integer", "double"),
      "range" = c("integer", "double")
    ),
    msg_main = msg_analyze_output
  )

  check_component_tibble(
    x = data$horns_results,
    name = "horns_results",
    dims = c(nrow(data$horns_results), 2L),
    col_names_types = list(
      "id" = "integer",
      "horns" = "double"
    ),
    msg_main = msg_analyze_output
  )

  min <- data$horns_metrics$min
  max <- data$horns_metrics$max

  data <- data$horns_results

  breaks_labels_x <- seq(from = min, to = max, by = 0.005)

  # Build the plot
  ggplot2::ggplot(data, ggplot2::aes(x = .data$horns)) +
    ggplot2::geom_bar(
      alpha = bar_alpha,
      fill = bar_color,
      position = ggplot2::position_dodge2()
    ) +
    ggplot2::scale_x_continuous(
      breaks = breaks_labels_x,
      labels = breaks_labels_x |> round(2)
    ) +
    ggplot2::labs(
      x = "Horns index",
      y = "Count in all horns index values"
    ) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}
