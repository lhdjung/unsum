
library(ggplot2)
library(dplyr)
library(unsum)
library(agrmt)

theme_set(theme_minimal(base_size = 12))

# Borrowed from scrutiny's internals
censor <- function(x, left, right) {
  x[x < left] <- left
  x[x > right] <- right
  x
}

dissention <- function(freqs, ...) 1 - consensus(freqs)


# This is needed in case none of the values has any observations on it, so the
# zeroes need to be added here. Also, the output is formatted consistently.
complete_scale_by_zeroes <- function(x, endpoint) {
  names_complete <- as.character(seq_len(endpoint))
  x_is_present <- names_complete %in% names(x)

  if (all(x_is_present)) {
    x <- as.integer(x)
    names(x) <- names_complete
    return(x)
  }

  x_new <- integer(endpoint)
  x_new[which(x_is_present)] <- x
  names(x_new) <- names_complete
  x_new
}



# Must be a whole number
endpoint <- 7

sd_all <- seq(from = 0, to = endpoint, by = 0.1)
n <- 500000


dispersion_by_metric <- function(fn) {    # sd_all, n, endpoint

  n_sds <- length(sd_all)

  freqs_all <- vector("list", n_sds)
  probs_all <- vector("list", n_sds)
  disp_all <- numeric(n_sds)

  for (i in seq_along(sd_all)) {
    freqs_current <- n |>
      rnorm(mean = 3, sd = sd_all[i]) |>
      round() |>
      censor(left = 1, right = endpoint) |>
      table() |>
      complete_scale_by_zeroes(endpoint)

    disp_all[i] <- fn(freqs_current, 1, endpoint)
  }

  disp_all
}


df1 <- tibble(
  sd = sd_all,
  horns = dispersion_by_metric(horns),
  # horns_rescaled = dispersion_by_metric(horns_rescaled),
  dissention = dispersion_by_metric(dissention)
)

df1


# Visualize the metrics, comparing them against each other as the SD rises
ggplot(df1, aes(x = sd)) +
  geom_point(aes(y = horns), color = "black") +
  # geom_point(aes(y = horns_rescaled), color = "royalblue2") +
  geom_point(aes(y = dissention), color = "brown2") +
  scale_x_continuous(
    breaks = 0:endpoint,
    labels = 0:endpoint
  ) +
  labs(
    x = "Standard deviation",
    y = "Dispersion metric",
    title = "Comparing measures of ordinal dispersion",
    subtitle = paste(
      "Black: horns index,",
      # "blue: rescaled horns index,",
      "red: dissention (i.e., 1 - consensus)"
    )
  )


# Uniform distribution ----------------------------------------------------

dissention_uniform <- function(scale_min, scale_max) {
  dissention(
    freqs = rep(1, length(scale_min:scale_max))
  )
}

dispersion_by_endpoint <- function(fn, x) {
  vapply(
    x,
    function(endpoint) fn(1, endpoint),
    numeric(1)
  )
}


# Create many different scale endpoints (i.e., `scale_max` values), but always
# start at 1, so that the endpoint is equal to the scale length
df2 <- tibble::tibble(
  scale_length = 2:20,
  horns_uniform = dispersion_by_endpoint(horns_uniform, scale_length),
  dissention_uniform = dispersion_by_endpoint(dissention_uniform, scale_length)
)


decimal_seq <- seq(from = 0.1, to = 1, by = 0.1)


# Visualize the decline of dispersion metrics as the number of scale points
# increases
ggplot(df2, aes(x = scale_length)) +
  geom_line(aes(y = horns_uniform), color = "black") +
  geom_line(aes(y = dissention_uniform), color = "brown2") +
  scale_x_continuous(breaks = df2$scale_length) +
  scale_y_continuous(
    breaks = decimal_seq,
    labels = decimal_seq
  ) +
  labs(
    x = "Number of scale points",
    y = "Dispersion in a uniform sample",
    title = "Comparing measures of ordinal dispersion in the uniform case",
    subtitle = paste(
      "Black: horns index,",
      "red: dissention (i.e., 1 - consensus)"
    )
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


print(df2, n = Inf)
