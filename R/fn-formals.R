# About this file ---------------------------------------------------------

# The functions in this file create, modify, or test a function's list of
# arguments. They are grouped into these sections:

# -- "Constructors" create or capture a list of arguments. The names of these
# functions all start on `formals_new_`.

# -- "Modifiers" change an existing list of arguments. Example usage:
# `your_function |> formals() |> formals_add("mode", .after = "data")`
# Most functions here are modifiers!

# -- "Predicates" run checks on a list of arguments and return logical vectors.
# Currently, there is only one predicate.

# -- "Helpers" are only needed by the other functions.

# If you use these functions as build helpers but not in the final binary, you
# could run `rm(list = ls(pattern = "^formals_"))` in a zzz.R file. This will
# remove all these functions before they can be included in the package binary.

# Dependencies: rlang and cli only, and cli is only used to throw errors. So
# they could be rewritten to remove the cli dependency, but this would make the
# errors less helpful. Some of the functions rely on other functions here.

# Constructors ------------------------------------------------------------

#' Get arguments from any function
#'
#' @description Wrapper around `formals()` with the same arguments but these
#' differences:
#'  - It doesn't return `NULL` for primitives. Instead, it returns the list of
#' "pseudo-arguments" that the primitive function would have if it was a
#' closure. For example, `args()` shows these pseudo-arguments but doesn't
#' directly return them.
#'  - It throws an error if the input is not a function.
#'
#' @param fun Any function.
#' @param envir See `?formals`.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Works on closures just like `formals()` does
#' formals_new_pseudo(colnames)
#'
#' # Also works on primitives, where `formals()` returns `NULL`
#' formals_new_pseudo(sum)
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


#' Create a list of arguments without defaults
#'
#' @description This is like a pairlist of name-value pairs but without values.
#'   Similar to `rlang::pairlist2()` when used like `pairlist2(a = , b = )`.
#'   However, the current function is more flexible because it can even create
#'   such lists programmatically from a string vector, without hard-coding the
#'   arguments.
#'
#' @param ... Strings that will become the new arguments.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Create a pairlist from individual strings
#' formals_new_without_defaults("x", "y", "z")
#'
#' # Create a pairlist from a string vector
#' formals_new_without_defaults(c("alpha", "beta"))
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

#' Add defaults to a list of arguments
#'
#' @description Insert defaults for existing arguments or replace old defaults
#'   by new ones.
#'
#' @param fmls Pairlist of arguments.
#' @param ... Name-value pairs of existing arguments and their new defaults.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' paste_custom <- paste
#'
#' # Add a default to one argument
#' formals(paste_custom) <- paste |>
#'   formals() |>
#'   formals_add_defaults(collapse = ", ")
#' paste_custom(letters)
#'
#' # Replace defaults for multiple arguments at once
#' formals(paste_custom) <- paste_custom |>
#'   formals() |>
#'   formals_add_defaults(sep = "_", collapse = "")
#' paste_custom(letters)
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


#' Set defaults for all arguments
#'
#' @description All arguments will get new defaults; you don't name the
#' arguments one by one. Instead, you supply either a single default used
#' throughout or a vector of as many defaults as there are arguments.
#'
#' @param fmls Pairlist of arguments.
#' @param new_defaults Vector with length 1 or the same length as `fmls`.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Set a single default for all arguments
#' formals_new_without_defaults("a", "b", "c") |>
#'   formals_add_defaults_all(new_defaults = TRUE)
#'
#' # Set a different default for each argument
#' formals_new_without_defaults("x", "y") |>
#'   formals_add_defaults_all(new_defaults = c(0, 1))
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
      "`new_defaults` must be length 1 or the length of `fmls` ({n_args}).",
      "x" = "It is actually length {n_new}."
    ))
  }

  names(out) <- names(fmls)

  as.pairlist(out)
}


#' Remove some defaults
#'
#' @description Defaults of the named arguments are removed, but the arguments
#' themselves are preserved. Use by passing strings (through the dots) that are
#' the names of the arguments you want to free from their defaults.
#'
#' @param fmls Pairlist of arguments.
#' @param ... Strings. Names of those arguments that should no longer have
#'   defaults.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Remove the default from a single argument
#' paste |> formals() |> formals_remove_defaults("sep")
#'
#' # Remove defaults from multiple arguments at once
#' paste |> formals() |> formals_remove_defaults("sep", "collapse")
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


#' Remove all defaults
#'
#' @description Strip the defaults of all arguments. The arguments themselves
#'   are preserved, so the function returns a pairlist with names but no values.
#'
#' @param fmls Pairlist of arguments.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Before
#' paste |> formals()
#'
#' # After
#' paste |> formals() |> formals_remove_defaults_all()
formals_remove_defaults_all <- function(fmls) {
  # Checking here redundantly because otherwise, any error message would
  # confusingly name `formals_new_without_defaults()` as the error's source
  formals_check_pairlist(fmls)
  formals_new_without_defaults(names(fmls))
}


#' Add arguments
#'
#' @description Extend a list of arguments. Use by supplying some combination of
#'   name-value pairs of new arguments with defaults, and / or strings with the
#'   names of new arguments that should not have any defaults.
#'
#'   For example, `my_formals |> formals_add("b", "c" = NULL, .after = "a")`
#'   will add `b` as an argument without a default and `c` as an argument with
#'   default `NULL`; in this order and after the existing argument `a`.
#'
#' @param fmls Pairlist of arguments.
#' @param ... New arguments (see above).
#' @param .before,.after Strings. Specify exactly one of these two arguments.
#'   Use it to name the one existing argument before or after which you want to
#'   place the new arguments.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Add an argument without a default after an existing one
#' mean.default |> formals() |> formals_add("weights", .after = "x")
#'
#' # Add an argument with a default before an existing one
#' colnames |> formals() |> formals_add(na.action = "omit", .before = "prefix")
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


#' Remove some arguments
#'
#' @description Remove certain arguments from the list of a function's
#'   arguments. Use by passing names of those arguments that should be removed.
#'
#' @param fmls Pairlist of arguments.
#' @param ... Strings. Names of existing arguments to remove.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Remove a single argument
#' mean.default |> formals() |> formals_remove("trim")
#'
#' # Remove multiple arguments at once
#' paste |> formals() |> formals_remove("sep", "collapse")
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

#' Do arguments have defaults?
#'
#' @description For each argument in a list of arguments, this says `TRUE` if
#'   the argument has a default, and `FALSE` if it does not.
#'
#' @param fmls Pairlist of arguments.
#'
#' @returns Logical vector with the same length as `fmls`. No `NA` elements.
#' @noRd
#'
#' @examples
#' # Check a function where some arguments have defaults
#' mean.default |> formals() |> formals_have_defaults()
#'
#' # All `FALSE` when no argument has a default
#' formals_new_without_defaults("a", "b") |> formals_have_defaults()
formals_have_defaults <- function(fmls) {
  formals_check_pairlist(fmls)

  defaults_are_missing <- logical(length(fmls))

  for (i in seq_along(fmls)) {
    current_arg <- fmls[[i]]
    defaults_are_missing[i] <- missing(current_arg)
  }

  !defaults_are_missing
}


# Helpers -----------------------------------------------------------------

#' Check whether object is pairlist
#'
#' @description Helper that throws an error if its argument is not a pairlist,
#'   i.e., the special type of object that a function's list of arguments
#'   consists of.
#'
#' @param x Some object; presumably a pairlist of arguments.
#'
#' @returns Nothing; might throw an error.
#' @noRd
#'
#' @examples
#' # Passes silently for a valid pairlist
#' formals_check_pairlist(formals(mean))
#'
#' # Throws an error for a regular list
#' try(formals_check_pairlist(list(a = 1)))
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


#' Get the index of a given argument
#'
#' @description Helper that throws an error if the position argument in
#' `formals_add()`, i.e., `.before` or `.after`, is not a length-1 string that
#' is the name of an existing argument. If the check passes, it returns the
#' index of that argument.
#'
#' @param names_old String. Vector of the names of the existing arguments.
#' @param name_next String (length 1). Value of `.before` or `.after`.
#'
#' @returns Integer (length 1).
#' @noRd
#'
#' @examples
#' # Returns the position of the named argument
#' formals_get_index(c("x", "na.rm", "trim"), "na.rm")
#'
#' # Throws an error if the name is not found
#' try(formals_get_index(c("x", "y"), "z"))
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
