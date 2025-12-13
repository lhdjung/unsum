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
  } else if (length(new_defaults) == 0) {
    cli::cli_abort("Need to name one or more existing arguments.")
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
  } else if (length(targets_all) == 0) {
    cli::cli_abort("Need to name one or more existing arguments.")
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
formals_add <- function(formals_fn, ..., .before = NULL, .after = NULL) {
  new_args_all <- list(...)

  # Unlike `base::names()`, `rlang::names2()` returns `""` for unnamed elements
  names_old <- rlang::names2(formals_fn)
  names_new <- rlang::names2(new_args_all)

  args_have_no_defaults <- names_new == ""
  offenders <- names_new[names_new %in% names_old]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only add arguments that don't exist yet.",
      "x" = "Already existing argument{?s}: {offenders}"
    ))
  } else if (length(new_args_all) == 0) {
    cli::cli_abort("Need to add one or more arguments.")
  }

  using_before <- !is.null(.before)
  using_after <- !is.null(.after)

  # Check that exactly one of `.before` and `.after` is a string that names an
  # existing argument, and the other one is `NULL`
  if (using_before && using_after) {
    cli::cli_abort("Can't specify both `.before` and `.after`.")
  } else if (!using_before && !using_after) {
    cli::cli_abort("Need to specify one of `.before` and `.after`.")
  } else if (using_before) {
    stopifnot(
      is.character(.before),
      length(.before) == 1,
      .before %in% names_old
    )
    index <- match(.before, names_old)
  } else {
    # `.after` remains
    stopifnot(
      is.character(.after),
      length(.after) == 1,
      .after %in% names_old
    )
    index <- match(.after, names_old)
  }

  # Insert each new argument into the list by checking its position and, if
  # needed, handling the no-default case
  for (i in seq_along(new_args_all)) {
    new_arg <- new_args_all[i]
    new_name <- names_new[i]
    has_no_default <- args_have_no_defaults[i]

    # Add temporary dummy default if needed because `c()` wants all elements to
    # be name-value pairs when it combines the new pairlist below. Redefine as a
    # string with value `"dummy"` and the name that is nested within `new_arg`.
    if (has_no_default) {
      new_arg <- `names<-`("dummy", new_arg[[1]])
    }

    # Update positions around which the new arg will be inserted, based on
    # current `index` value that is incremented after each loop iteration
    if (using_before) {
      neighbor_low <- index - 1
      neighbor_high <- index
    } else {
      # I.e., if `using_after`
      neighbor_low <- index
      neighbor_high <- index + 1
    }

    # Insert the current new argument into the list of arguments. The second
    # part of the existing list -- after the new argument -- might not exist
    # because the new argument is inserted right at the end of the list.
    # Therefore, `NULL` is inserted instead if there is no second part. This
    # problem can't occur on the lower end because the index only ever
    # increments, and because R can inherently handle the lower end better.
    formals_fn <- c(
      formals_fn[seq_len(neighbor_low)],
      new_arg,

      if (neighbor_high > length(formals_fn)) {
        NULL
      } else {
        formals_fn[neighbor_high:length(formals_fn)]
      }
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
  } else if (length(targets_all) == 0) {
    cli::cli_abort("Need to name one or more existing arguments.")
  }

  should_be_removed <- names(formals_fn) %in% targets_all
  formals_fn[!should_be_removed]
}
