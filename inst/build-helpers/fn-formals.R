# Replace the default of an existing argument by a new default
formals_change_defaults <- function(formals_fn, ...) {
  new_defaults <- list(...)

  if (length(new_defaults) == 0) {
    return(formals_fn)
  }

  offenders <-
    names(new_defaults)[!(names(new_defaults) %in% names(formals_fn))]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only choose new defaults for existing arguments.",
      "x" = "Non-existing argument{?s}: {offenders}"
    ))
  }

  formals_fn[names(formals_fn) %in% names(new_defaults)] <- new_defaults
  formals_fn
}


# Extend the list of a function's arguments
formals_add <- function(formals_fn, ..., .after) {
  new_formals <- list(...)
  offenders <- names(new_formals)[names(new_formals) %in% names(formals_fn)]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only add arguments that don't exist yet.",
      "x" = "Already existing argument{?s}: {offenders}"
    ))
  }

  if (!is.character(.after)) {
    cli::cli_abort("`.after` must be a string.")
  }

  .after <- match(.after, names(formals_fn))

  c(
    formals_fn[seq_len(.after)],
    new_formals,
    formals_fn[(.after + 1):length(formals_fn)]
  )
}


# Remove selected arguments from the list of a function's arguments
formals_remove <- function(formals_fn, ...) {
  formals_to_remove <- c(...)
  offenders <- formals_to_remove[!(formals_to_remove %in% names(formals_fn))]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only remove existing arguments.",
      "x" = "Non-existing argument{?s}: {offenders}"
    ))
  }

  should_be_removed <- names(formals_fn) %in% formals_to_remove
  formals_fn[!should_be_removed]
}


# This helper is tailored to `generate_from_mean_sd_n()`. It takes this
# function's formal arguments, removes those that are not needed in its wrappers
# such as `closure_generate()`, and returns the remaining ones. In this way, the
# list of arguments is always up to date with a single source of truth, without
# manually copying much code around.
formals_adapt_generator <- function(fn_basic) {
  fn_basic |>
    formals() |>
    formals_remove(
      "technique",
      "rounding_error_mean",
      "rounding_error_sd"
    )
}
