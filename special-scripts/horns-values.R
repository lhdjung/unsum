
library(ggplot2)
library(dplyr)

theme_set(theme_minimal(base_size = 12))

# Borrowed from scrutiny's internals
censor <- function(x, left, right) {
  x[x < left] <- left
  x[x > right] <- right
  x
}


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


# df1 <- tibble::tibble(
#   frequencies = sd_all |>
#     vapply(
#       function(x) rnorm(n, mean = 3, sd = x),
#       numeric(n)
#     ) |>
#     table() |>
#     as.list()
# )

# probs_all <- list(
#   c(0.5, 0, 0, 0, 0.5),
#   c(0.4, 0.1, 0, 0.1, 0.4),
#   c(0.3, 0.2, 0, 0.2, 0.3),
#   c(0.1, 0.2, 0.4, 0.2, 0.1),
#   c(0, 0.2, 0.6, 0.2, 0),
#   c(0, 0, 1, 0, 0)
# )


# Must be a whole number -- maximal SD to include
endpoint <- 6

sd_all <- seq(from = 0, to = endpoint, by = 0.1)
n_sds <- length(sd_all)

freqs_all <- vector("list", n_sds)
probs_all <- vector("list", n_sds)
horns_all <- numeric(n_sds)
horns_rescaled_all <- numeric(n_sds)

n <- 100000


for (i in seq_along(sd_all)) {

  freqs_current <- n |>
    rnorm(mean = 3, sd = sd_all[i]) |>
    round() |>
    censor(left = 1, right = endpoint) |>
    table() |>
    complete_scale_by_zeroes(endpoint)

  horns_all[i] <- horns(freqs_current, 1, endpoint)
  horns_rescaled_all[i] <- horns_rescaled(freqs_current, 1, endpoint)

  # freqs_all[[i]] <- freqs_current
  #
  # probs_all[[i]] <- endpoint |>
  #   seq_len() |>
  #   sample(
  #     size = n,
  #     replace = TRUE,
  #     prob = freqs_current / sum(freqs_current)
  #   ) |>
  #   table()


  # names_complete <- as.character(seq_len(endpoint))
  #
  # if (identical(names(freqs_current), names_complete)) {
  #   freqs_current <- as.integer(freqs_current)
  #   names(freqs_current) <- names_complete
  # } else {
  #   values_complete <- integer(endpoint)
  #   names(values_complete) <- names_complete
  #   values_complete[names_complete %in% names(freqs_current)] <- freqs_current
  #   freqs_current <- values_complete
  # }
  #
  # probs_current <- freqs_current / sum(freqs_current)
  #
  # if (!near(sum(probs_current), 1)) {
  #   cli::cli_abort("All `probs_current` elements must sum up to 1.")
  # }
  #
  # freqs_all[[i]] <- probs_current
  #
  # sample_current <- endpoint |>
  #   seq_len() |>
  #   sample(
  #     size = n,
  #     replace = TRUE,
  #     prob = probs_current
  #   ) |>
  #   table()
}

df1 <- tibble(
  sd = sd_all,
  horns = horns_all,
  horns_rescaled = horns_rescaled_all
)


ggplot(df1, aes(x = sd, y = horns)) +
  geom_point() +
  geom_point(aes(y = horns_rescaled_all), color = "red") +
  labs(
    x = "Standard deviation",
    y = "Horns index"
  )



# horns_results_all <- probs_all |>
#   lapply(function(x) sample(1:5, size = 5000, replace = TRUE, prob = x)) |>
#   lapply(
#     function(x) {
#       out <- tibble::as_tibble(table(x))
#       x_is_present <- as.character(1:5) %in% out$x
#       freqs <- if (all(x_is_present)) {
#         out$n
#       } else {
#         n_new <- integer(5)
#         n_new[which(x_is_present)] <- out$n
#         n_new
#       }
#       horns(freqs, 1, 5)
#     }
#   ) |>
#   as.numeric()



# Uniform distribution ----------------------------------------------------

# Create many different scale endpoints (i.e., `scale_max` values), but always
# start at 1, so that the endpoint is equal to the scale length
df2 <- tibble::tibble(
  scale_length = 2:20,
  horns_uniform = vapply(
    scale_length,
    function(endpoint) horns_uniform(1, endpoint),
    numeric(1)
  )
)

decimal_seq <- seq(from = 0.1, to = 1, by = 0.1)

ggplot(df2, aes(x = scale_length, y = horns_uniform)) +
  geom_line() +
  scale_x_continuous(breaks = df2$scale_length) +
  scale_y_continuous(
    breaks = decimal_seq,
    labels = decimal_seq
  ) +
  labs(
    x = "Number of scale points",
    y = "Horns index of a uniform sample"
  ) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

print(df2, n = Inf)
