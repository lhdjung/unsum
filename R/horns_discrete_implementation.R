#' Horns Index with Sample Size Corrections
#'
#' @description Calculates the horns index accounting for sample size effects.
#'   Two versions are provided: one that accounts for finite sample constraints (h)
#'   and one that uses theoretical limits (h*).
#'
#' @details The horns index measures variance as a proportion of maximum possible
#'   variance. For finite samples, two effects modify the achievable maximum:
#'   1. Sample variance formula (Bessel's correction) affects all N
#'   2. Odd sample sizes prevent perfect 50-50 splits between extremes
#'
#' @param freqs Numeric vector with frequencies of binned observations
#' @param scale_min Numeric. Minimum value of the scale
#' @param scale_max Numeric. Maximum value of the scale
#' @param use_sample_variance Logical. If TRUE, uses sample variance (n-1).
#'   If FALSE, uses population variance (n). Default is TRUE to match
#'   standard statistical practice and CLOSURE methodology.
#' @param return_details Logical. If TRUE, returns detailed components
#'
#' @return If return_details = FALSE: the horns index (numeric, 0 to 1)
#'         If return_details = TRUE: a list with components:
#'         - h: the finite-sample horns index
#'         - h_star: the theoretical (infinite-sample) horns index
#'         - observed_var: observed variance
#'         - max_var_finite: maximum variance for this specific N
#'         - max_var_theoretical: theoretical maximum (Popoviciu bound)
#'         - n: sample size
#'         - variance_formula: "sample" or "population"
#'
#' @examples
#' # Example with odd N = 27 on 1-5 scale
#' freqs_odd <- c(13, 0, 0, 0, 14)  # Close to maximum variance
#' horns(freqs_odd, 1, 5)
#'
#' # Compare h and h* for small sample
#' freqs_small <- c(3, 0, 0, 0, 2)  # N=5
#' horns(freqs_small, 1, 5, return_details = TRUE)
#'
#' @noRd

horns <- function(
  freqs,
  scale_min,
  scale_max,
  use_sample_variance = TRUE,
  return_details = FALSE
) {
  # Input validation
  if (!is.numeric(freqs) || any(freqs < 0)) {
    stop("freqs must be a non-negative numeric vector")
  }
  if (!is.numeric(scale_min) || !is.numeric(scale_max)) {
    stop("scale_min and scale_max must be numeric")
  }
  if (scale_min >= scale_max) {
    stop("scale_min must be less than scale_max")
  }

  # Calculate basic parameters
  n <- sum(freqs)
  k <- length(freqs)

  # Check consistency
  if (k != length(scale_min:scale_max)) {
    stop(sprintf(
      "Length of freqs (%d) doesn't match scale length (%d to %d)",
      k,
      scale_min,
      scale_max
    ))
  }

  # Handle edge cases
  if (n == 0) {
    warning("No observations (n = 0). Returning NA.")
    return(NA)
  }
  if (n == 1) {
    warning("Only one observation. Variance is undefined. Returning 0.")
    if (return_details) {
      return(list(
        h = 0,
        h_star = 0,
        observed_var = 0,
        max_var_finite = 0,
        max_var_theoretical = (k - 1)^2 / 4,
        n = n,
        variance_formula = ifelse(use_sample_variance, "sample", "population")
      ))
    }
    return(0)
  }

  # Step 1: Calculate observed variance
  scale_points <- seq_along(freqs)
  freqs_relative <- freqs / n
  observed_mean <- sum(scale_points * freqs_relative)

  # Sum of squared deviations
  ss <- sum(freqs * (scale_points - observed_mean)^2)

  if (use_sample_variance) {
    observed_var <- ss / (n - 1) # Sample variance
  } else {
    observed_var <- ss / n # Population variance
  }

  # Step 2: Calculate theoretical maximum (Popoviciu bound)
  # This is the limit as n approaches infinity
  max_var_theoretical <- (k - 1)^2 / 4

  # Step 3: Calculate maximum variance for finite N
  # We need to consider both odd/even split and variance formula

  if (n %% 2 == 0) {
    # Even N: perfect split possible
    n1 <- n2 <- n / 2
    mean_at_max <- (k + 1) / 2 # Exactly at midpoint

    # Sum of squared deviations for maximum configuration
    ss_max <- n1 * (1 - mean_at_max)^2 + n2 * (k - mean_at_max)^2
    # This simplifies to: n * (k-1)^2/4
  } else {
    # Odd N: uneven split required
    n1 <- (n - 1) / 2
    n2 <- (n + 1) / 2
    mean_at_max <- (n1 * 1 + n2 * k) / n

    # Sum of squared deviations for maximum configuration
    ss_max <- n1 * (1 - mean_at_max)^2 + n2 * (k - mean_at_max)^2
    # After algebra, this equals: n * (k-1)^2/4 * (1 - 1/n^2)
  }

  if (use_sample_variance) {
    max_var_finite <- ss_max / (n - 1) # Sample variance of max configuration
  } else {
    max_var_finite <- ss_max / n # Population variance of max configuration
  }

  # Step 4: Calculate both versions of horns index
  h <- observed_var / max_var_finite # Finite-sample version
  h_star <- observed_var / max_var_theoretical # Theoretical version

  # For sample variance, h_star needs adjustment
  if (use_sample_variance) {
    # h* uses the limiting case, but observed variance is still sample-based
    # We need to convert to what it would be with population variance
    observed_var_pop <- observed_var * (n - 1) / n
    h_star <- observed_var_pop / max_var_theoretical
  }

  # Ensure indices don't exceed 1 due to numerical precision
  h <- min(h, 1)
  h_star <- min(h_star, 1)

  if (return_details) {
    return(list(
      h = h,
      h_star = h_star,
      observed_var = observed_var,
      max_var_finite = max_var_finite,
      max_var_theoretical = max_var_theoretical,
      n = n,
      variance_formula = ifelse(use_sample_variance, "sample", "population"),
      is_even_n = (n %% 2 == 0),
      bessel_factor = ifelse(use_sample_variance, n / (n - 1), 1),
      odd_n_factor = ifelse(n %% 2 == 0, 1, 1 - 1 / n^2)
    ))
  }

  return(h)
}


#' Calculate h* (Theoretical Horns Index)
#'
#' @description The theoretical horns index that doesn't account for finite sample
#'   constraints. This represents the limiting case as n approaches infinity.
#'
#' @inheritParams horns
#'
#' @noRd

horns_star <- function(freqs, scale_min, scale_max) {
  n <- sum(freqs)
  k <- length(freqs)

  # Calculate variance using population formula (limiting case)
  scale_points <- seq_along(freqs)
  freqs_relative <- freqs / n
  observed_mean <- sum(scale_points * freqs_relative)
  observed_var <- sum(freqs_relative * (scale_points - observed_mean)^2)

  # Theoretical maximum (Popoviciu bound)
  max_var_theoretical <- (k - 1)^2 / 4

  # h* is simply the ratio using theoretical maximum
  h_star <- observed_var / max_var_theoretical

  return(min(h_star, 1))
}


#' Compare Finite-Sample and Theoretical Horns Indices
#'
#' @description Shows how h and h* differ, especially for small samples
#'
#' @inheritParams horns
#' @param show_components Logical. If TRUE, shows how maximum variance
#'   is affected by sample size and odd/even constraints
#'
#' @noRd

compare_horns_versions <- function(
  freqs,
  scale_min,
  scale_max,
  show_components = TRUE
) {
  # Get detailed results using sample variance (standard practice)
  details <- horns(
    freqs,
    scale_min,
    scale_max,
    use_sample_variance = TRUE,
    return_details = TRUE
  )

  n <- details$n
  k <- length(freqs)

  # Create comparison
  cat("Horns Index Comparison\n")
  cat(rep("=", 40), "\n", sep = "")
  cat("Sample size (N):", n, ifelse(n %% 2 == 0, "(even)", "(odd)"), "\n")
  cat("Scale points (k):", k, "\n")
  cat("\nIndices:\n")
  cat("  h (finite-sample):", sprintf("%.4f", details$h), "\n")
  cat("  h* (theoretical):", sprintf("%.4f", details$h_star), "\n")
  cat("  Ratio h/h*:", sprintf("%.4f", details$h / details$h_star), "\n")

  if (show_components) {
    cat("\nMaximum Variance Components:\n")
    cat(
      "  Theoretical (Popoviciu):",
      sprintf("%.4f", details$max_var_theoretical),
      "\n"
    )
    cat("  Finite-sample:", sprintf("%.4f", details$max_var_finite), "\n")

    if (details$variance_formula == "sample") {
      cat("\nFactors affecting maximum (using sample variance):\n")
      cat(
        "  Bessel correction: n/(n-1) =",
        sprintf("%.4f", details$bessel_factor),
        "\n"
      )
      if (!details$is_even_n) {
        cat(
          "  Odd-N geometric factor: 1 - 1/n^2 =",
          sprintf("%.4f", details$odd_n_factor),
          "\n"
        )
      }
    }
  }

  # Interpretation
  cat("\nInterpretation:\n")
  if (abs(details$h - details$h_star) < 0.001) {
    cat("  h and h* are virtually identical (large sample)\n")
  } else if (details$h > details$h_star) {
    cat("  h > h* because finite-N maximum is less than theoretical maximum\n")
  }

  if (details$h > 0.8) {
    cat("  ! High h suggests distribution is near maximum dispersion\n")
  }

  return(invisible(details))
}


#' Demonstrate How Maximum Variance Changes with N
#'
#' @description Shows how the achievable maximum variance depends on sample size
#'
#' @param k Number of scale points
#' @param n_values Vector of sample sizes to compare
#'
#' @noRd

demonstrate_sample_size_effects <- function(
  k = 5,
  n_values = c(5, 10, 20, 50, 100, 1000),
  shape = c("horns", "uniform", "all_equal")
) {
  shape <- arg_match_in_export(shape)

  cat("Maximum Variance by Sample Size\n")
  cat("Scale: 1 to", k, "\n")
  cat(rep("=", 50), "\n", sep = "")

  results <- tibble::tibble(
    n = n_values,
    is_even = (n_values %% 2 == 0),
    max_var_sample = NA,
    max_var_pop = NA,
    bessel_factor = n_values / (n_values - 1),
    odd_factor = ifelse(n_values %% 2 == 0, 1, 1 - 1 / n_values^2),
    h = NA,
    h_star = NA,
    bias = NA
  )

  for (i in seq_along(n_values)) {
    n <- n_values[i]

    # Create maximum variance configuration
    freqs_in_shape <- numeric(k)

    # Frequency distribution depends on `shape`
    switch(
      shape,
      "horns" = {
        if (n %% 2 == 0) {
          freqs_in_shape[1] <- n / 2
          freqs_in_shape[k] <- n / 2
        } else {
          freqs_in_shape[1] <- (n - 1) / 2
          freqs_in_shape[k] <- (n + 1) / 2
        }
        shape_description <- "polarized (maximum-variance) distribution"
      },
      "uniform" = {
        freqs_in_shape <- rep(n / k, k)
        shape_description <- "uniform distribution"
      },
      "all_equal" = {
        freqs_in_shape[1] <- n
        shape_description <- "zero variance, all values equal"
      },
      cli::cli_abort("Internal error: unhandled `shape` type.")
    )

    # Calculate using both variance formulas
    scale_points <- 1:k
    mean_val <- sum(freqs_in_shape * scale_points) / n
    ss <- sum(freqs_in_shape * (scale_points - mean_val)^2)

    results$max_var_sample[i] <- ss / (n - 1)
    results$max_var_pop[i] <- ss / n

    # Calculate h and h* to get bias
    details <- horns(
      freqs_in_shape,
      scale_min = 1,
      scale_max = k,
      use_sample_variance = TRUE,
      return_details = TRUE
    )

    results$h[i] <- details$h
    results$h_star[i] <- details$h_star
    results$bias[i] <- details$h - details$h_star
  }

  # Theoretical maximum
  theoretical_max <- (k - 1)^2 / 4

  # Display results
  cat("\n")
  cat(sprintf(
    "%-6s %-6s %-12s %-12s %-8s\n",
    "N",
    "Even?",
    "Max(sample)",
    "Max(pop)",
    "-> Theor"
  ))
  cat(rep("-", 50), "\n", sep = "")

  for (i in 1:nrow(results)) {
    cat(sprintf(
      "%-6d %-6s %-12.6f %-12.6f %s\n",
      results$n[i],
      ifelse(results$is_even[i], "Yes", "No"),
      results$max_var_sample[i],
      results$max_var_pop[i],
      ifelse(i == nrow(results), sprintf("-> %.6f", theoretical_max), "")
    ))
  }

  cat(
    "\nNote: As N increases, both converge to theoretical maximum =",
    theoretical_max,
    "\n"
  )
  cat("Sample variance converges from above, population variance from below.\n")

  # Create ggplot of small-sample bias
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    results_even <- results[results$is_even, ]
    results_odd <- results[!results$is_even, ]

    # print(results_even)
    # print(results_odd)

    p <- ggplot2::ggplot(results, ggplot2::aes(x = n, y = .data$bias)) +

      # Even-N line
      ggplot2::geom_line(
        linewidth = 1,
        color = "royalblue",
        data = results_even
      ) +

      # Odd-N line
      ggplot2::geom_line(linewidth = 1, color = "red", data = results_odd) +

      ggplot2::geom_point(size = 3) +
      ggplot2::geom_label(
        ggplot2::aes(label = round(.data$bias, 2)),
        vjust = -0.5
      ) +
      ggplot2::scale_x_continuous(
        breaks = results$n,
        labels = results$n
      ) +
      ggplot2::scale_y_continuous(limits = c(0, max(results$bias) + 0.02)) +
      ggplot2::labs(
        x = "Sample size",
        y = "Small-sample bias (*h - h**)",
        title = "Small-sample bias in the horns index",
        subtitle = paste0(
          "*k* = ",
          k,
          " scale points, ",
          shape_description,
          "; blue: even N, red: odd N"
        )
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        title = ggtext::element_markdown(),
        axis.title = ggtext::element_markdown(),
        panel.grid.minor.x = ggplot2::element_blank()
      )

    print(p)
  } else {
    warning("ggplot2 is required to display the bias plot")
  }

  return(invisible(results))
}


# Example usage
if (FALSE) {
  # Set to TRUE to run examples

  # Example 1: Small odd sample
  cat("\n=== Example 1: N=5 on 1-5 scale ===\n")
  freqs1 <- c(2, 0, 0, 0, 3) # Close to maximum variance
  compare_horns_versions(freqs1, 1, 5)

  # Example 2: Medium even sample
  cat("\n=== Example 2: N=30 on 1-7 scale ===\n")
  freqs2 <- c(15, 0, 0, 0, 0, 0, 15) # Perfect maximum variance
  compare_horns_versions(freqs2, 1, 7)

  # Example 3: Sample size effects
  cat("\n=== Example 3: How maximum variance changes with N ===\n")
  demonstrate_sample_size_effects(
    k = 7,
    n_values = 2:20,
    # n_values = c(2, 5, 10, 20, 50, 100, 500),
    shape = "horns"
  )
}
