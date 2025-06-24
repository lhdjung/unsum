
# Replace the default of an existing argument by a new default.
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
