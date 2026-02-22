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

lov <- function(freqs, ...) Leik(freqs)


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
n <- 50000


dispersion_by_metric <- function(fn) {
  # sd_all, n, endpoint

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
  horns_corrected = dispersion_by_metric(horns_corrected),
  horns_rescaled = dispersion_by_metric(horns_rescaled),
  leik = dispersion_by_metric(lov),
  dissention = dispersion_by_metric(dissention)
)

df1


# Visualize the metrics, comparing them against each other as the SD rises
ggplot(df1, aes(x = sd)) +
  geom_point(aes(y = horns), color = "black") +
  geom_point(aes(y = horns_corrected), color = "red") +
  geom_point(aes(y = horns_rescaled), color = "royalblue2") +
  geom_point(aes(y = leik), color = "blue") +
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
      "blue: LOV,",
      "red: dissention (i.e., 1 - consensus)"
    )
  )


# Uniform distribution ----------------------------------------------------

lov_uniform <- function(scale_min, scale_max) {
  1 |> rep(length(scale_min:scale_max)) |> lov()
}

dissention_uniform <- function(scale_min, scale_max) {
  1 |> rep(length(scale_min:scale_max)) |> dissention()
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
  horns_uniform_cont = 1 / 3,
  horns_uniform = dispersion_by_endpoint(horns_uniform, scale_length),
  lov_uniform = dispersion_by_endpoint(lov_uniform, scale_length),
  dissention_uniform = dispersion_by_endpoint(dissention_uniform, scale_length)
)

df2

decimal_seq <- seq(from = 0.1, to = 1, by = 0.1)

# # Points on the x-axis
# length_max <- 1 + nrow(df2)

text_label_size <- 4.5

color_horns_uniform <- "black"
color_lov_uniform <- "blue"
color_dissention_uniform <- "brown2"
color_horns_uniform_cont <- "purple"


# Visualize the decline of dispersion metrics as the number of scale points
# increases
ggplot(df2, aes(x = scale_length)) +
  geom_line(aes(y = horns_uniform_cont), color = color_horns_uniform_cont) +
  geom_line(aes(y = horns_uniform), color = color_horns_uniform) +
  geom_line(aes(y = lov_uniform), color = color_lov_uniform) +
  geom_line(aes(y = dissention_uniform), color = color_dissention_uniform) +
  # geom_hline(yintercept = 1 / 3, color = color_horns_uniform_cont) +

  # Text labels
  annotate(
    geom = "text",
    x = 15,
    y = 0.57,
    label = "LOV",
    size = text_label_size,
    color = color_lov_uniform
  ) +
  annotate(
    geom = "text",
    x = 16,
    y = 0.44,
    label = "Dissention (i.e., 1 - consensus)",
    size = text_label_size,
    color = color_dissention_uniform
  ) +
  annotate(
    geom = "text",
    x = 11,
    y = 0.44,
    label = "Horns index",
    size = text_label_size,
    color = color_horns_uniform
  ) +
  annotate(
    geom = "text",
    x = 5.5,
    y = 0.39,
    label = "Horns index of the\ncontinuous uniform distribution = 1 / 3",
    size = text_label_size,
    color = color_horns_uniform_cont
  ) +

  # Rest of the plot
  scale_x_continuous(breaks = df2$scale_length) +
  scale_y_continuous(
    breaks = decimal_seq,
    labels = decimal_seq
  ) +
  labs(
    x = "Number of scale points",
    y = "Dispersion in a uniform sample",
    title = "Comparing measures of ordinal dispersion in the uniform case"
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


print(df2, n = Inf)


# Investigating patterns in Leik's ordinal variation (LOV) for uniform
# distributions
lov_seq <- df2$lov_uniform
lov_diff <- numeric(length(lov_seq))

for (i in seq_along(lov_diff)) {
  if (i == 1) {
    next
  }
  lov_diff[i] <- lov_seq[i] - lov_seq[i - 1]
}

lov_diff[1] <- NaN
lov_diff <- round(lov_diff, 10)


df2 |>
  select(scale_length, lov_uniform) |>
  mutate(lov_diff)


# Spelling out Leik's cumulative relative frequencies

freqs_cumul <- seq(0.1, 1, by = 0.1)
freqs_cumul_diff <- vapply(freqs_cumul, function(f) min(f, 1 - f), numeric(1))

freqs_cumul
freqs_cumul_diff

k <- length(freqs_cumul)
peak <- floor(k / 2)

# TODO: CHECK IF CORRECT; THIS IS SUPPOSED TO BE
sum(freqs_cumul[seq_len(peak)])

# Comparing h and h* ------------------------------------------------------

relative <- c(0.29, 0.18, 0.06, 0.18, 0.29)

df3 <- tibble(
  n = 20:30,
  freqs = lapply(n, function(x) x * relative),
  h = vapply(freqs, function(x) horns_corrected(x, 1, 5), numeric(1)),
  h_star = vapply(freqs, function(x) horns(x, 1, 5), numeric(1)),
)


absolute <- c(2, 5, 5, 8, 8)
relative <- absolute / sum(absolute)

df4 <- tibble(
  inflation = seq(from = 0.1, to = 20, by = 0.2),
  freqs = lapply(inflation, function(x) x * absolute),
  mean = vapply(freqs, mean, numeric(1)),
  n = vapply(freqs, function(x) sum(x), numeric(1)),
  h = vapply(freqs, function(x) horns_corrected(x, 1, 5), numeric(1)),
  h_star = vapply(freqs, function(x) horns(x, 1, 5), numeric(1)),
  bias = abs(h - h_star)
)

df4 <- df4 |>
  mutate(id = as.double(seq_len(nrow(df4))), .before = inflation)


ggplot(df4, aes(x = n)) +
  geom_line(aes(y = h), color = "black") +
  geom_line(aes(y = h_star), color = "royalblue1") +
  labs(x = "Sample size", y = "Horns index") +
  scale_y_continuous(limits = c(0.66, 0.665))

##
#
#
#
#
#
#
#
#
#
#
#
#
