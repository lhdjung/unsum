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
#'   `closure_horns_histogram()` draws a quick barplot to reveal the
#'   distribution of horns values. The scale is fixed between 0 and 1.
#'
#' @param data For `closure_horns_analyze()`, a list returned by
#'   `closure_generate()`. For `closure_horns_histogram()`, a list returned by
#'   `closure_horns_analyze()`.
#' @param bar_alpha Numeric (length 1). Opacity of the bars. Default is `0.8`.
#' @param bar_binwidth Width of the bins that divide up the x-axis, passed on to
#'   [`ggplot2::geom_histogram()`]. Default is `0.0025`.
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
#'   `closure_horns_histogram()` returns a ggplot object.
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
#' closure_horns_histogram(data_horns)

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

closure_horns_histogram <- function(
  data,
  bar_alpha = 0.8,
  bar_color = "#5D3FD3",
  bar_binwidth = 0.0025,
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
    msg_main = msg_analyze_output,
    n = 1
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
    msg_main = msg_analyze_output,
    n = 1
  )

  check_component_tibble(
    x = data$horns_results,
    name = "horns_results",
    dims = c(nrow(data$horns_results), 2L),
    col_names_types = list(
      "id" = "integer",
      "horns" = "double"
    ),
    msg_main = msg_analyze_output,
    n = 1
  )

  min <- data$horns_metrics$min
  max <- data$horns_metrics$max

  data <- data$horns_results

  # Construct the plot
  ggplot2::ggplot(data) +
    ggplot2::geom_histogram(
      ggplot2::aes(x = horns),
      alpha = bar_alpha,
      fill = bar_color,
      binwidth = bar_binwidth
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      oob = function(x, limits) x
    ) +
    ggplot2::labs(
      x = "Horns index (h)",
      y = "Count in all horns index values"
    ) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}
