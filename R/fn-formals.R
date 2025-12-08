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


# Remove the defaults of one or more existing arguments, but keep the arguments
# themselves. Use by passing strings (through the dots) that are the names of
# the arguments you want to free from their defaults.
formals_remove_defaults <- function(formals_fn, ...) {
  targets_all <- c(...)

  names_all <- names(formals_fn)
  offenders <- targets_all[!targets_all %in% names_all]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only remove defaults of existing arguments.",
      "x" = "Non-existing argument{?s}: {offenders}"
    ))
  }

  # Replace each target formal argument by a version of itself that does not
  # have a default
  for (target in targets_all) {
    index <- match(target, names_all)

    # Parse and evaluate a call to rlang's pairlist constructor that allows for
    # missing arguments. This produces a pairlist of an argument named after
    # `target` but without a default, which then replaces the version of itself
    # that does have a default.
    formals_fn[index] <- eval(rlang::parse_expr(paste0(
      "rlang::pairlist2(",
      target,
      " = )"
    )))
  }

  formals_fn
}


# Extend the list of a function's arguments. Use by supplying some combination
# of name-value pairs of arguments and defaults and / or strings with the names
# of new arguments that should not have any defaults. For example,
# `list_of_formals |> formals_add(a = 5, "b")` will add `a` as an argument with
# default `5`, and `b` as an argument without a default; in this order.
formals_add <- function(formals_fn, ..., .after) {
  new_args_all <- list(...)
  names_args_new <- names(new_args_all)
  names_args_old <- names(formals_fn)

  offenders <- names(new_args_all)[names(new_args_all) %in% names_args_old]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only add arguments that don't exist yet.",
      "x" = "Already existing argument{?s}: {offenders}"
    ))
  }

  if (!is.character(.after) || !.after %in% names_args_old) {
    cli::cli_abort("`.after` must be a string that names an existing argument.")
  }

  index <- match(.after, names(formals_fn))

  for (i in seq_along(new_args_all)) {
    new_arg <- new_args_all[i]
    new_name <- names_args_new[i]

    has_no_default <- is.null(new_name)

    # Add temporary dummy default if needed because `c()` wants all elements to
    # be name-value pairs when it combines the new pairlist below
    if (has_no_default) {
      # Redefine because `new_arg` only has a (nested) value, not a name
      new_arg <- new_arg[[1]]
      stand_in <- c(x = "dummy")
      # The "value" of `new_arg` should be the name of the new argument
      names(stand_in) <- new_arg
      new_arg <- stand_in
    }

    # Insert the current new argument into the list of arguments
    formals_fn <- c(
      formals_fn[seq_len(index)],
      new_arg,
      formals_fn[(index + 1):length(formals_fn)]
    )

    # Remove temporary dummy default
    if (has_no_default) {
      formals_fn <- formals_remove_defaults(formals_fn, names(new_arg))
    }

    # Increment because the next new argument should go after the current one
    index <- index + 1
  }

  formals_fn
}


# Remove selected arguments from the list of a function's arguments
formals_remove <- function(formals_fn, ...) {
  targets_all <- c(...)
  offenders <- targets_all[!targets_all %in% names(formals_fn)]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only remove existing arguments.",
      "x" = "Non-existing argument{?s}: {offenders}"
    ))
  }

  should_be_removed <- names(formals_fn) %in% targets_all
  formals_fn[!should_be_removed]
}
