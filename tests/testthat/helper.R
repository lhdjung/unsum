# Reading CLOSURE data from disk can lead to spurious differences in attributes;
# specifically, in pointers. However, what matters when comparing two data
# frames produced by different CLOSURE implementations is only the values, not
# any transitory details about the way R stores them in memory.

# Test whether two objects are identical except for their attributes. In other
# words, when the attributes are removed, is the rest identical between the two?
identical_except_attributes <- function(x, y) {
  identical(
    x = `attributes<-`(x, NULL),
    y = `attributes<-`(y, NULL)
  )
}


# Given two data frames, does each pair of columns contain the same values? This
# is tested by sorting the columns first, then comparing them. CLOSURE results
# are hard to predict, and different correct implementations can lead to
# differently sorted columns. The mark of correctness, then, is whether the
# columns are identical after being ordered equally. This assumes the same
# number and names of columns. With `message = TRUE`, it will print which column
# pair is the first unequal one if the result is `FALSE`.
identical_sorted_cols <- function(x, y, message = FALSE) {
  if (ncol(x) != ncol(y)) {
    cli::cli_abort("Different numbers of columns.")
  }
  if (!identical(colnames(x), colnames(y))) {
    cli::cli_abort("Different column names.")
  }
  for (n in seq_len(ncol(x))) {
    if (!identical(sort(x[[n]]), sort(y[[n]]))) {
      if (message) {
        message(paste("Different at", n))
      }
      return(FALSE)
    }
  }
  TRUE
}


# Count the decimal places of a length-1 numeric or string vector
decimal_places_scalar <- function(x, sep = "\\.") {
  if (is.na(x)) {
    return(NA_integer_)
  }

  hit <- regmatches(x, regexpr(paste0("(?<=", sep, ")\\d+"), x, perl = TRUE))

  if (length(hit) == 0L) {
    0L
  } else {
    nchar(hit)
  }
}


count_wrong_stats <- function(data) {
  name_fn_all <- c("mean", "sd")

  offenders <- name_fn_all[name_fn_all %in% names(data@results)]
  if (length(offenders) > 0) {
    cli::cli_abort(c(
      "Can't add columns that already exist.",
      "x" = "Already present as {offenders} column{?s} in the \"results\"
      tibble:",
      "x" = paste(paste0("\"", offenders, "\""), collapse = ", ")
    ))
  }

  which_unequal <- rep(list(NULL), length(name_fn_all))
  names(which_unequal) <- name_fn_all

  for (name_fn in name_fn_all) {
    input_stat <- data@inputs[[name_fn]]
    input_stat_num <- as.numeric(input_stat)

    digits <- decimal_places_scalar(input_stat)

    # Which samples, if any, don't conform to the input summary stats?
    which_unequal_current <- data@results$sample |>
      vapply(
        function(sample) {
          # Get the summary function by name and call it on the current sample
          result <- eval(call(name_fn, sample))

          # Use the poor man's default `reround()` to assert that either
          # possible reconstructed value matches the original input statistic
          roundwork::round_up(result, digits) == input_stat_num ||
            roundwork::round_down(result, digits) == input_stat_num
        },
        logical(1)
      ) |>
      # Negate to then get the indices where neither reconstructed value matches
      call_on(\(x) !x) |>
      which()

    if (length(which_unequal_current) > 0) {
      which_unequal[[name_fn]] <- which_unequal_current
    }
  }

  list(
    original = data@inputs,
    recomputed = tibble::tibble(
      input = name_fn_all,
      count = vapply(which_unequal, length, integer(1)),
      locations = which_unequal
    )
  )
}

# Take the output of `count_wrong_stats()` and check if any stats are wrong
any_wrong_stats <- function(maybe_wrong) {
  locations <- maybe_wrong$recomputed$locations
  locations_are_correct <- vapply(locations, is.null, logical(1))

  if (all(locations_are_correct)) {
    return(FALSE)
  }

  locations_wrong <- maybe_wrong$recomputed[!locations_are_correct, ]

  cli::cli_alert_danger("Recomputed summary stats diverge from the inputs!")
  print(maybe_wrong)
  cli::cli_alert_danger("These values are different:\n\n")

  # Show exactly which samples fail to yield the input summary stats:
  for (i in nrow(locations_wrong)) {
    case <- locations_wrong[i, ]

    msg_locations <- case$locations[[1]] |>
      as.numeric() |>
      paste0(collapse = ", ")

    cli::cli_alert_info(paste0("Statistic: \"", case$input, "\""))
    cli::cli_alert_info(paste0("Locations: ", msg_locations))
    cat("\n")
  }

  TRUE
}


# Sort the columns of a single data frame. Return as tibble because `lapply()`
# returns a bare list, and unsum only ever uses tibbles for data frames.
sort_cols <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.")
  }
  x |>
    lapply(sort) |>
    tibble::as_tibble()
}
