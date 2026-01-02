# About this file ---------------------------------------------------------

# The functions in this file create, modify, or test a function's list of
# arguments. They are grouped into these sections:

# -- "Constructors" create or capture a list of arguments. The names of these
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
    cli::cli_abort("Arguments of this function must be unnamed strings.")
  }

  # Single string with names formatted like "a  = , b  = , c  = "
  names_equal_empty <- paste(names_all, " = ", collapse = ", ")

  # See comment in `formals_remove_defaults()` which works the same way
  eval(rlang::parse_expr(paste0(
    "rlang::pairlist2(",
    names_equal_empty,
    ")"
  )))
}


# Modifiers ---------------------------------------------------------------

# Insert defaults for existing arguments or replace old defaults by new ones
formals_add_defaults <- function(fmls, ...) {
  formals_check_pairlist(fmls)

  new_defaults <- list(...)

  if (length(new_defaults) == 0) {
    return(fmls)
  }

  offenders <-
    names(new_defaults)[!(names(new_defaults) %in% names(fmls))]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only choose new defaults for existing arguments.",
      "x" = "Non-existing argument{?s}: {offenders}"
    ))
  } else if (length(new_defaults) == 0) {
    cli::cli_abort("Need to name one or more existing arguments.")
  }

  fmls[names(fmls) %in% names(new_defaults)] <- new_defaults

  as.pairlist(fmls)
}


# Set defaults for all arguments. This must be either a single default used
# throughout or a vector of as many defaults as there are arguments.
formals_add_defaults_all <- function(fmls, new_defaults) {
  formals_check_pairlist(fmls)

  n_args <- length(fmls)
  n_new <- length(new_defaults)

  if (n_new == 1) {
    out <- rep(new_defaults, n_args)
  } else if (n_new == n_args) {
    out <- new_defaults
  } else {
    cli::cli_abort(c(
      "`new_defaults` must be length 1 or as long as `fmls`.",
      "x" = "It is actually length {n_new}."
    ))
  }

  names(out) <- names(fmls)

  as.pairlist(out)
}


# Remove the defaults of one or more existing arguments, but keep the arguments
# themselves. Use by passing strings (through the dots) that are the names of
# the arguments you want to free from their defaults.
formals_remove_defaults <- function(fmls, ...) {
  formals_check_pairlist(fmls)

  targets_all <- c(...)

  names_all <- names(fmls)
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
    fmls[index] <- eval(rlang::parse_expr(paste0(
      "rlang::pairlist2(",
      target,
      " = )"
    )))
  }

  # Arguments are stored in pairlists, not regular lists
  as.pairlist(fmls)
}


# Simple wrapper that removes all defaults from a list of arguments
formals_remove_defaults_all <- function(fmls) {
  formals_check_pairlist(fmls)
  formals_new_without_defaults(names(fmls))
}


# Extend the list of a function's arguments. Use by supplying some combination
# of name-value pairs of arguments and defaults and / or strings with the names
# of new arguments that should not have any defaults. For example,
# `list_of_formals |> formals_add(a = 5, "b")` will add `a` as an argument with
# default `5`, and `b` as an argument without a default; in this order.
formals_add <- function(fmls, ..., .before = NULL, .after = NULL) {
  formals_check_pairlist(fmls)

  new_args_all <- list(...)

  # Unlike `base::names()`, `rlang::names2()` returns `""` for unnamed elements
  names_old <- rlang::names2(fmls)
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
    index <- formals_get_index(names_old, .before)
  } else {
    # `.after` remains
    index <- formals_get_index(names_old, .after)
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
    fmls <- as.pairlist(c(
      fmls[seq_len(neighbor_low)],
      new_arg,

      if (neighbor_high > length(fmls)) {
        NULL
      } else {
        fmls[neighbor_high:length(fmls)]
      }
    ))

    # Remove temporary dummy default
    if (has_no_default) {
      fmls <- formals_remove_defaults(fmls, names(new_arg))
    }

    # Increment because the next new argument should go after the current one
    index <- index + 1
  }

  as.pairlist(fmls)
}


# Remove selected arguments from the list of a function's arguments
formals_remove <- function(fmls, ...) {
  formals_check_pairlist(fmls)

  targets_all <- c(...)
  offenders <- targets_all[!targets_all %in% names(fmls)]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only remove existing arguments.",
      "x" = "Non-existing argument{?s}: {offenders}"
    ))
  } else if (length(targets_all) == 0) {
    cli::cli_abort("Need to name one or more existing arguments.")
  }

  should_be_removed <- names(fmls) %in% targets_all

  as.pairlist(fmls[!should_be_removed])
}


# Predicates --------------------------------------------------------------

# For each argument in a list of arguments, this says `TRUE` if the argument has
# a default, and `FALSE` if it does not
formals_have_defaults <- function(fmls) {
  formals_check_pairlist(fmls)

  are_missing <- logical(length(fmls))

  for (i in seq_along(fmls)) {
    current_arg <- fmls[[i]]
    are_missing[i] <- missing(current_arg)
  }

  !are_missing
}


# Helpers -----------------------------------------------------------------

# Helper that throws an error if its argument is not a pairlist, i.e., the
# special type of object that a function's list of arguments consists of
formals_check_pairlist <- function(x) {
  if (typeof(x) != "pairlist") {
    cli::cli_abort(
      c(
        "`fmls` must be the list of a function's arguments.",
        "x" = "Its actual type is: {typeof(x)}",
        "i" = "Get a function's arguments using `formals(your_function)`."
      ),
      call = rlang::caller_env()
    )
  }
}


# Helper that throws an error if the position argument in `formals_add()`, i.e.,
# `.before` or `.after`, is not a length-1 string that is the name of an
# existing argument. If the check passes, it returns the index of that argument.
formals_get_index <- function(names_old, name_next) {
  if (
    is.character(name_next) &&
      length(name_next) == 1 &&
      name_next %in% names_old
  ) {
    return(match(name_next, names_old))
  }

  name <- deparse(substitute(name_next))

  cli::cli_abort(
    "`{name}` must be a single string that names an existing argument.",
    call = rlang::caller_env()
  )
}
