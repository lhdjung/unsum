# About this file ---------------------------------------------------------

# The functions in this file create, modify, or test a function's list of
# arguments. They are grouped into these sections:

# -- "Constructors" create a list of arguments from scratch. The names of these
# functions all start on `formals_new_`.
# -- "Modifiers" change an existing list of arguments. Example usage:
# `your_function |> formals() |> formals_add("mode", .after = "data")`
# -- "Predicates" run checks on a list of arguments and return logical vectors.
# -- "Helpers" are only needed by the other functions.

# If you use them as build helpers and don't need them in the final binary, you
# could run `rm(list = ls(pattern = "^formals_"))` in a zzz.R file. This will
# remove all these functions before they can be included in the package binary.

# Dependencies: rlang and cli only, and cli is only used to throw errors. So
# they could be rewritten to remove the cli dependency, but this would make the
# errors less helpful. Some of the functions rely on other functions here.

# Constructors ------------------------------------------------------------

# Alternative to `formals()` with the same arguments but these differences:
# -- It doesn't return `NULL` for primitives. Instead, it returns the list of
# "pseudo-arguments" that the primitive function would have if it was a closure.
# E.g., `args()` shows these pseudo-arguments but doesn't directly return them.
# -- It throws an error if `fun` is not a function.
formals_new_pseudo <- function(fun, envir = parent.frame(2)) {
  if (is.primitive(fun)) {
    formals(args(fun), envir)
  } else if (is.function(fun)) {
    formals(fun, envir)
  } else {
    cli::cli_abort(c(
      "`fun` must be a function.",
      "x" = "Its actual type is: {typeof(fun)}"
    ))
  }
}


# Create a pairlist of arguments without defaults. Similar to, and based on,
# `rlang::pairlist2()` when used like `pairlist2(a = , b = )`. However, the
# current function is more flexible because it can even create such lists
# programmatically from a string vector, without hard-coding the arguments.
formals_new_without_defaults <- function(...) {
  names_all <- c(...)

  if (!is.character(names_all) || !all(rlang::names2(names_all) == "")) {
    cli::cli_abort(
      "Arguments of `formals_new_without_defaults()` must be unnamed strings."
    )
  }

  # Prepare a makeshift list -- not yet a pairlist -- of arguments
  out <- vector("list", length(names_all))
  names(out) <- names_all

  # Transform the list into a pairlist and remove its `NULL` defaults
  formals_remove_defaults(out, names_all)
}


# Modifiers ---------------------------------------------------------------

# Replace the default of an existing argument by a new default
formals_change_defaults <- function(formals_fn, ...) {
  check_pairlist(formals_fn)

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

  as.pairlist(formals_fn)
}


# Remove the defaults of one or more existing arguments, but keep the arguments
# themselves. Use by passing strings (through the dots) that are the names of
# the arguments you want to free from their defaults.
formals_remove_defaults <- function(formals_fn, ...) {
  check_pairlist(formals_fn)

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

  # Arguments are stored in pairlists, not regular lists
  as.pairlist(formals_fn)
}


# Simple wrapper that removes all defaults from a list of arguments
formals_remove_defaults_all <- function(formals_fn) {
  check_pairlist(formals_fn)
  formals_remove_defaults(formals_fn, names(formals_fn))
}


# Extend the list of a function's arguments. Use by supplying some combination
# of name-value pairs of arguments and defaults and / or strings with the names
# of new arguments that should not have any defaults. For example,
# `list_of_formals |> formals_add(a = 5, "b")` will add `a` as an argument with
# default `5`, and `b` as an argument without a default; in this order.
formals_add <- function(formals_fn, ..., .before = NULL, .after = NULL) {
  check_pairlist(formals_fn)

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
    formals_fn <- as.pairlist(c(
      formals_fn[seq_len(neighbor_low)],
      new_arg,

      if (neighbor_high > length(formals_fn)) {
        NULL
      } else {
        formals_fn[neighbor_high:length(formals_fn)]
      }
    ))

    # Remove temporary dummy default
    if (has_no_default) {
      formals_fn <- formals_remove_defaults(formals_fn, names(new_arg))
    }

    # Increment because the next new argument should go after the current one
    index <- index + 1
  }

  as.pairlist(formals_fn)
}


# Remove selected arguments from the list of a function's arguments
formals_remove <- function(formals_fn, ...) {
  check_pairlist(formals_fn)

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

  as.pairlist(formals_fn[!should_be_removed])
}


# Predicates --------------------------------------------------------------

# For each argument in a list of arguments, this says `TRUE` if the argument has
# a default, and `FALSE` if it does not
formals_have_defaults <- function(formals_fn) {
  check_pairlist(formals_fn)

  out <- logical(length(formals_fn))

  for (i in seq_along(formals_fn)) {
    current_arg <- formals_fn[[i]]

    if (!missing(current_arg)) {
      out[i] <- TRUE
    }
  }

  out
}


# Helpers -----------------------------------------------------------------

# Helper that throws an error if its argument is not a pairlist, i.e., the
# special type of object that a function's list of arguments consist of.
check_pairlist <- function(x) {
  if (typeof(x) != "pairlist") {
    cli::cli_abort(
      c(
        "`formals_fn` must be the list of a function's arguments.",
        "x" = "Its actual type is: {typeof(x)}",
        "i" = "Get a function's arguments using `formals(your_function)`."
      ),
      call = rlang::caller_env()
    )
  }
}
