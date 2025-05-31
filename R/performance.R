
estimate_runtime <- function(mean, sd, n, scale_min, scale_max) {

  # Scale range is THE dominant factor - empirically verified
  range_size <- scale_max - scale_min + 1

  # 1. Initial combinations (quadratic in range_size)
  initial_combinations <- (range_size * (range_size + 1)) / 2

  # 2. Scale range factor (the key insight from your scale_max=5 vs 7 test)
  # This has exponential impact through the search tree
  if (range_size <= 3) {
    scale_factor <- 0.1      # trivial cases
  } else if (range_size <= 5) {
    scale_factor <- 1.0      # baseline
  } else if (range_size <= 7) {
    scale_factor <- 50.0     # under a minute
  } else if (range_size <= 9) {
    scale_factor <- 500.0    # extrapolated
  } else {
    scale_factor <- 5000.0   # very large ranges
  }

  # 3. Sample size factor (tree depth effect)
  if (n <= 20) {
    n_factor <- 0.5
  } else if (n <= 50) {
    n_factor <- 1.0          # baseline
  } else if (n <= 100) {
    n_factor <- 3.0
  } else if (n <= 200) {
    n_factor <- 10.0
  } else {
    n_factor <- 50.0
  }

  # 4. SD constraint factor (loose constraints = less pruning = harder)
  # Your sd=2.0 case was loose and took time
  if (sd <= 0.5) {
    sd_factor <- 0.1         # very tight = easy
  } else if (sd <= 1.0) {
    sd_factor <- 0.5         # tight
  } else if (sd <= 1.5) {
    sd_factor <- 1.0         # moderate
  } else if (sd <= 2.0) {
    sd_factor <- 2.0         # loose
  } else {
    sd_factor <- 5.0         # very loose
  }

  # 5. Mean extremeness (extreme means are harder to achieve)
  scale_center <- (scale_min + scale_max) / 2
  mean_distance_from_center <- abs(mean - scale_center)
  max_distance <- (scale_max - scale_min) / 2
  mean_extremeness <- mean_distance_from_center / max_distance

  if (mean_extremeness < 0.2) {
    mean_factor <- 1.0       # central means are easier
  } else if (mean_extremeness < 0.5) {
    mean_factor <- 1.5
  } else {
    mean_factor <- 3.0       # extreme means are harder
  }

  # 6. Base complexity calculation
  base_complexity <- scale_factor * n_factor * sd_factor * mean_factor

  # 7. Convert to log scale for interpretability
  log10(base_complexity)
}
