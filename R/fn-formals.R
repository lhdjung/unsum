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

# -- "Getters" can be used in conjunction with other functions here.

# -- "Helpers" are only needed internally by the other functions.

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
#' formals_new_safe(colnames)
#'
#' # Also works on primitives, where `formals()` returns `NULL`
#' formals_new_safe(sum)
formals_new_safe <- function(fun, envir = parent.frame(2)) {
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

  # Create a list of empty arguments and name them after the inputs
  out <- rep(list(rlang::missing_arg()), length(names_all))
  names(out) <- names_all

  # Arguments are stored in pairlists, not regular lists
  as.pairlist(out)
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

  offenders <- names(new_defaults)[!(names(new_defaults) %in% names(fmls))]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only choose new defaults for existing arguments.",
      "x" = "Non-existing argument{?s}: {offenders}"
    ))
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

  # Length-zero objects need to be wrapped in a list, which also fits length-1
  # objects because they are not recycled
  if (n_new < 2) {
    out <- rep(list(new_defaults), n_args)
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

  # Replace the target defaults by the missing argument object
  indices_all <- which(names_all %in% targets_all)
  fmls[indices_all] <- list(rlang::missing_arg())

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
#'   place the new arguments. Accepts `formals_first()` or `formals_last()` to
#'   select the first or last argument, regardless of its name.
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

  have_no_defaults <- names_new == ""
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

  names_new[have_no_defaults] <- new_args_all[have_no_defaults]
  new_args_all[have_no_defaults] <- list(rlang::missing_arg())

  names(new_args_all) <- names_new

  # Determine where to split the existing formals
  split <- if (using_before) index - 1 else index

  # Construct the full pairlist in one shot: [before | new | after].
  # Convert pairlist subsets to regular lists so that `c()` preserves elements
  # with missing values (no defaults), which `c()` on pairlists would drop.
  as.pairlist(c(
    as.list(fmls[seq_len(split)]),
    new_args_all,
    if (split < length(fmls)) as.list(fmls[(split + 1):length(fmls)])
  ))
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


#' Rename arguments
#'
#' @description Rename one or more arguments. Uses the `dplyr::rename()`
#'   convention: each name-value pair in the dots has the new name on the left
#'   and the old name (a string) on the right.
#'
#'   For example, `fmls |> formals_rename(new = "old")` renames the argument
#'   called `old` to `new`. Order and defaults are preserved.
#'
#' @param fmls Pairlist of arguments.
#' @param ... Name-value pairs: `new_name = "old_name"`.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Rename a single argument
#' mean.default |> formals() |> formals_rename(value = "x")
#'
#' # Rename multiple arguments at once
#' paste |> formals() |> formals_rename(separator = "sep", joiner = "collapse")
formals_rename <- function(fmls, ...) {
  formals_check_pairlist(fmls)

  mapping <- c(...)

  if (
    !is.character(mapping) ||
      is.null(names(mapping)) ||
      any(names(mapping) == "")
  ) {
    cli::cli_abort("All arguments must be named: `new_name = \"old_name\"`.")
  }

  if (length(mapping) == 0) {
    cli::cli_abort("Need to rename one or more arguments.")
  }

  current <- names(fmls)
  old_names <- unname(mapping)
  new_names <- names(mapping)

  offenders <- old_names[!old_names %in% current]

  if (length(offenders) > 0) {
    offenders <- paste0("\"", offenders, "\"")
    cli::cli_abort(c(
      "Can only rename existing arguments.",
      "x" = "Non-existing argument{?s}: {offenders}"
    ))
  }

  # New names must not collide with existing names that are not being renamed
  remaining <- current[!current %in% old_names]
  collisions <- new_names[new_names %in% remaining]

  if (length(collisions) > 0) {
    collisions <- paste0("\"", collisions, "\"")
    cli::cli_abort(c(
      "New names must not clash with existing argument names.",
      "x" = "Already existing name{?s}: {collisions}"
    ))
  }

  current[match(old_names, current)] <- new_names
  names(fmls) <- current

  as.pairlist(fmls)
}


#' Reorder arguments
#'
#' @description Rearrange arguments in a new order. Named arguments are moved to
#'   the front in the order supplied; any remaining arguments follow in their
#'   original order. Defaults are preserved.
#'
#' @param fmls Pairlist of arguments.
#' @param ... Strings. Names of existing arguments in the desired order. May be
#'   a subset; unmentioned arguments keep their original relative order and are
#'   placed after the named ones.
#'
#' @returns Pairlist of arguments.
#' @noRd
#'
#' @examples
#' # Move one argument to the front; the rest stay in their original order
#' mean.default |> formals() |> formals_reorder("na.rm")
#'
#' # Supply all names for a complete reorder
#' paste |> formals() |> formals_reorder("recycle0", "collapse", "sep", "...")
formals_reorder <- function(fmls, ...) {
  formals_check_pairlist(fmls)

  new_order <- c(...)

  if (!is.character(new_order) || !all(rlang::names2(new_order) == "")) {
    cli::cli_abort("Arguments of this function must be unnamed strings.")
  }

  if (length(new_order) == 0) {
    cli::cli_abort("Need to name one or more arguments.")
  }

  current <- names(fmls)
  extra_names <- new_order[!new_order %in% current]

  if (length(extra_names) > 0) {
    extra_names <- paste0("\"", extra_names, "\"")
    cli::cli_abort(c(
      "Can only reorder existing arguments.",
      "x" = "Non-existing argument{?s}: {extra_names}"
    ))
  }

  # Named arguments go first; the rest follow in their original order
  rest <- current[!current %in% new_order]
  full_order <- c(new_order, rest)

  as.pairlist(fmls[match(full_order, current)])
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

  # Loop needed here because `missing()` doesn't work with `vapply()` etc.
  for (i in seq_along(fmls)) {
    current_arg <- fmls[[i]]
    defaults_are_missing[i] <- missing(current_arg)
  }

  !defaults_are_missing
}


# Getters -----------------------------------------------------------------

#' Select the first or last argument
#'
#' These can be used inside of `formals_add()` to specify `.before` or `.after`
#' to make sure you always select the first or last argument.
#'
#' @returns String (length 1). It is empty but will instruct `formals_add()` to
#'   pick the first or last argument, respectively.
#' @noRd
#'
#' @examples
formals_first <- function() {
  out <- ""
  class(out) <- c("formals_first", "character")
  out
}

formals_last <- function() {
  out <- ""
  class(out) <- c("formals_last", "character")
  out
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
  if (!is.pairlist(x)) {
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
#'   `formals_add()`, i.e., `.before` or `.after`, is not a length-1 string that
#'   is the name of an existing argument. If the check passes, it returns the
#'   index of that argument.
#'
#' @param names_old String. Vector of the names of the existing arguments.
#' @param name_next String (length 1). Value of `.before` or `.after`. Accepts
#'   `formals_first()` or `formals_last()` to select the first or last argument.
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
  if (!is.character(name_next) || length(name_next) != 1) {
    name <- deparse(substitute(name_next))
    cli::cli_abort(
      c(
        "`{name}` must be a single string that matches an existing argument.",
        i = "Name that argument or use `formals_first()` / `formals_last()`."
      ),
      call = rlang::caller_env()
    )
  }

  name_target <- if (name_next %in% names_old) {
    name_next
  } else if (inherits(name_next, "formals_first")) {
    names_old[1]
  } else if (inherits(name_next, "formals_last")) {
    names_old[length(names_old)]
  } else {
    cli::cli_abort(
      c(
        "String must match an existing argument.",
        i = "Name that argument or use `formals_first()` / `formals_last()`."
      ),
      call = rlang::caller_env()
    )
  }

  match(name_target, names_old)
}
