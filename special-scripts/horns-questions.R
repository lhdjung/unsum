# Take an integer-ish vector of frequencies and recreate the original
# distribution it describes. In other words, this is the reverse operation of
# `tabulate()`. It uses an integer scale starting on 1, but for the use cases of
# unsum, other scales would work the same way.
freqs_to_values <- function(freqs) {
  out <- vector("list", length(freqs))
  values <- seq_along(freqs)

  for (i in values) {
    out[[i]] <- rep(values[i], freqs[i])
  }

  unlist(out, use.names = FALSE)
}


freqs_h_min <- c(300, 0, 0, 0, 0)
freqs_h_max <- c(150, 0, 0, 0, 150)


freqs_h_min |> horns(1, 5)
freqs_h_min |> freqs_to_values() |> var()
freqs_h_min |> var()


freqs_h_max |> horns(1, 5)
freqs_h_max |> freqs_to_values() |> var()
freqs_h_max |> var()
