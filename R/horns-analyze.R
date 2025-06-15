#' Horns index for each CLOSURE sample
#'
#' @description Following up on [`closure_generate()`], you can call
#'   `closure_horns_analyze()` to compute the horns index for each individual
#'   sample and calculate summary statistics on the distribution of these
#'   indices. See [`horns()`] for the metric itself.
#'
#'   See [horns_plot] for follow-up visualizations.
#'
#' @param data List returned by [`closure_generate()`].
#'
#' @details The results of `closure_generate()` already include a `"horns"`
#'   metric that is computed from the frequencies of scale values in the mean
#'   sample. This is different from the `"mean"` metric found here, which is the
#'   mean of the horns indices of all samples. These two values can differ
#'   because the horns index is based on squared deviations, which can result in
#'   non-linearities.
#'
#'   The `"mad"` column is created by [`stats::mad()`], but it does not rely on
#'   this function's default of adjusting the result via multiplication by a
#'   constant (about 1.48). This assumes a normal distribution, which is not
#'   generally the case with horns index values. Here, the constant is set to
#'   `1`.
#'
#' @return Named list of five tibbles (data frames):
#'   - **closure_generate_inputs**: Adopted from the input.
#'   - **horns_metrics**: Summary statistics of the distribution of horns index
#'   values:
#'     - `mean`, `uniform`: arithmetic mean.
#'     - `uniform`: horns value of a hypothetical uniform distribution. Equal to
#'   `horns_uniform` from the results of `closure_generate()`.
#'     - `sd`: double. Standard deviation.
#'     - `cv`: double. Coefficient of variation, i.e., `sd / mean`.
#'     - `mad`: double. Median absolute deviation; see [`stats::mad()`] but also
#'   details here.
#'     - `min`, `median`, `max`: double. Minimum, median, and maximum horns
#'   index.
#'     - `range`: double. Equal to `max - min`.
#'   - **frequency_horns_min**, **frequency_horns_max**: Frequency tables of the
#'   samples with the minimum or maximum horns values. They are structured like
#'   the `frequency` table in the output of `closure_generate()`. However, note
#'   that the `f_absolute` column here can include values from multiple samples,
#'   so it is better to focus on `f_average` or `f_relative`. These two refer to
#'   the average sample from the subset of samples with the minimum or maximum
#'   horns value.
#'   - **horns_results**:
#'     - `id`: integer. Uniquely identifies each horns index, just like their
#'   corresponding samples in `closure_generate()`.
#'     - `horns`: double. Horns index for each individual sample.
#'
#' @include horns.R plot.R utils.R
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
#' closure_horns_analyze(data)

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

    # Using the internal helper instead of the exported `horns()` because the
    # latter conducts a number of checks that slow it down by about 10x compared
    # to the helper, which would often lead to long runtimes in this loop.
    horns_values[i] <- horns_internal(
      freqs = f_absolute,
      scale_min = scale_min,
      scale_max = scale_max
    )
  }

  mean <- mean(horns_values)
  sd <- sd(horns_values)
  min <- min(horns_values)
  max <- max(horns_values)

  inputs <- list(data$inputs)

  # Anticipating a future parametrization of the technique's name
  names(inputs) <- "closure_generate_inputs"

  freqs_horns_min_max <- vector("list", 2)

  # For each extreme of the distribution of horns values (min and max), identify
  # the samples with this extreme horns value. Then create a frequency table for
  # all the values in this subset of samples, as in `closure_generate()`.
  for (i in 1:2) {
    extreme <- c(min, max)[i]

    indices <- extreme |>
      near(horns_values) |>
      which()

    freqs_horns_min_max[[i]] <- summarize_frequencies(
      results = data$results$sample[indices],
      scale_min = data$inputs$scale_min,
      scale_max = data$inputs$scale_max,
      samples_all = length(indices)
    )
  }

  names(freqs_horns_min_max) <- c(
    "frequency_horns_min",
    "frequency_horns_max"
  )

  inputs <- list(data$inputs)
  names(inputs) <- "closure_generate_inputs"

  # Combine the list of `closure_generate()` inputs with a new list that
  # contains the horns-specific statistics
  c(
    inputs,
    list(
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
      )
    ),
    freqs_horns_min_max,
    list(
      horns_results = tibble::new_tibble(
        x = list(
          id = seq_len(n_samples_all),
          horns = horns_values
        ),
        nrow = n_samples_all
      )
    )
  )
}

