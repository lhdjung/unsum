
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


# Sort the columns of a single data frame. Return as tibble because `lapply()`
# returns a bare list, and unsum only ever uses tibbles for data frames.
sort_cols <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.")
  }
  x %>%
    lapply(sort) %>%
    tibble::as_tibble()
}


# Transform unsum's CLOSURE results into the "n"-column format of the CSV files
# made by closure-core's test harness or the original Python implementation
format_n_cols <- function(data) {
  check_closure_combine(data)
  data$results$combination %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    t() %>%
    tibble::as_tibble(.name_repair = function(x) paste0("n", seq_along(x)))
}

