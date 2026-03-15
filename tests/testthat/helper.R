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

  scale_by <- switch(data$inputs$technique, "SPRITE" = 100, 1)

  offenders <- name_fn_all[name_fn_all %in% names(data$results)]
  if (length(offenders) > 0) {
    cli::cli_abort(c(
      "Can't add columns that already exist.",
      "x" = "Already present as {offenders} column{?s} in the \"results\"
      tibble:",
      "x" = paste(paste0("\"", offenders, "\""), collapse = ", ")
    ))
  }

  which_unequal <- NULL |>
    list() |>
    rep(length(name_fn_all)) |>
    setNames(name_fn_all)

  for (name_fn in name_fn_all) {
    input_stat <- data$inputs[[name_fn]]
    input_stat_num <- as.numeric(input_stat)

    digits <- decimal_places_scalar(input_stat)

    # Which samples, if any, don't conform to the input summary stats?
    which_unequal_current <- data$results$sample |>
      vapply(
        function(sample) {
          sample_current <- sample / scale_by

          # Get the summary function by name and call it on the current sample
          result <- eval(call(name_fn, sample_current))

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
    original = data$inputs,
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


# Takes results of SPRITE and CLOSURE based on the same summary statistics, and
# checks that each SPRITE result is contained in the CLOSURE results
is_contained_in <- function(
  subset,
  superset,
  stop_at_first = TRUE,
  scaling_by = 100,
  name_subset = "SPRITE",
  name_superset = "CLOSURE"
) {
  # Inputs must be comparable, i.e., based on the same summary statistics
  if (!identical_except_attributes(subset$inputs[-1], superset$inputs[-1])) {
    on.exit({
      cli::cli_alert_info("Alleged subset statistics:")
      print(subset$inputs)
      cli::cli_alert_info("Alleged superset statistics:")
      print(superset$inputs)
    })

    cli::cli_abort(c(
      "TEST CANNOT BE RUN.",
      `!` = "Result sets must be based on the same summary statistics.",
      x = "`subset` and `superset` are not comparable."
    ))
  }

  n_samples_subset <- nrow(subset$results)
  maybe_contained <- TRUE

  superset_samples <- superset$results$sample |>
    lapply(sort) |>
    lapply(\(x) x * scaling_by)

  # Go through each "subset" sample and check that it's actually contained in
  # the superset of results
  for (i in seq_len(n_samples_subset)) {
    current_subset_sample <- subset$results[i, ]$sample[[1]] |>
      sort() |>
      list()

    # Happy path: skip to next element
    if (current_subset_sample %in% superset_samples) {
      next
    }

    # Unhappy path: prepare error (the default) or warning
    maybe_contained <- FALSE

    if (stop_at_first) {
      msg_action <- "Aborting"
      msg_appendix <- c(
        i = "Use `stop_at_first = FALSE` to see all failing samples."
      )
    } else {
      msg_action <- "Failing"
      msg_appendix <- NULL
    }

    msg_complete <- c(
      "Failed to find all {name_subset} samples in {name_superset} results.",
      x = "{msg_action} at {name_subset} sample number {i}.",
      msg_appendix
    )

    cli::cli_alert_danger("Offending \"subset\" sample:")
    print(dput(current_subset_sample))

    if (stop_at_first) {
      cli::cli_abort(msg_complete)
    } else {
      cli::cli_alert_danger(msg_complete)
    }
  }

  # Returned if either `TRUE`, or both this and `stop_at_first` are `FALSE`
  maybe_contained
}


# Test CLOSURE / SPRITE results for being empty. Assumes that `data` is output
# of a generator or reader function but doesn't check this, so use with care.
is_empty <- function(data) {
  data$metrics_main$samples_all == 0
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
