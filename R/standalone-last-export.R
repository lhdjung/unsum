# ---
# repo: lhdjung/unsum
# file: standalone-last-export.R
# last-updated: 2026-02-23
# license: https://unlicense.org
# imports: [rlang, cli]
# ---

# Throw an error that names the top-level exported, user-called function as the
# source; e.g., "Error in `exported_fn()`", not "Error in `internal_helper()`".
abort_in_export <- function(...) {
  cli::cli_abort(
    message = c(...),
    call = caller_env_last_export(),
    .envir = parent.frame()
  )
}


# Similar to `abort_in_export()` but as an alternative to `match.arg()` /
# `rlang::arg_match()`. Advantage: If this function throws an error, it will
# name the exported function called by the user as the source of the problem.
# Arguments shared with `rlang::arg_match()` work as there.
arg_match_in_export <- function(arg, values = NULL, multiple = FALSE) {
  arg_expr <- rlang::enexpr(arg)
  arg_name <- as.character(arg_expr)

  # If values not provided, extract from the calling function's formals
  if (is.null(values)) {
    parent_formals <- formals(sys.function(sys.parent()))

    if (!arg_name %in% names(parent_formals)) {
      cli::cli_abort(
        "Internal error: {.arg {arg_name}} not found in calling function."
      )
    }

    values <- eval(parent_formals[[arg_name]], envir = parent.frame())
  }

  val <- rlang::eval_bare(arg_expr, env = rlang::caller_env())

  # Early escape hatch for performance in the typical use case
  if (!multiple && is.character(val) && length(val) == 1L && val %in% values) {
    return(val)
  }

  # This is retained for non-typical cases and for its characteristic error msg
  rlang::arg_match(
    arg = val,
    values = values,
    multiple = multiple,
    error_arg = arg_name,
    error_call = caller_env_last_export()
  )
}


# Find the environment of the exported function at the top of the call stack,
# i.e., the last or outermost exported function that was called. This is useful
# as a helper within `abort_in_export()` so that the function that was called by
# the user is named in the error message as the site where the error occurred.
caller_env_last_export <- function(package_name = NULL) {
  # If `package_name` is not provided, try detect it in the package environment
  if (is.null(package_name)) {
    pkg_env <- parent.env(environment())
    if (isNamespace(pkg_env)) {
      package_name <- getNamespaceName(pkg_env)
    } else {
      cli::cli_abort(c(
        "Could not determine package name.",
        "i" = "Please provide the `package_name` argument."
      ))
    }
  }

  # Get all frames and calls on the call stack
  frames <- sys.frames()
  calls <- sys.calls()

  # Get the namespace of the package
  ns <- tryCatch(
    getNamespace(package_name),
    error = function(e) {
      cli::cli_abort(
        "Package \"{package_name}\" not found or not loaded.",
        "x" = "Original error:",
        "x" = "{e}"
      )
    }
  )

  # Get list of exported functions from the package
  exports <- getNamespaceExports(ns)

  # Find the last (most recent in call stack) exported function
  for (i in seq_along(calls)) {
    fn <- calls[[i]][[1]]

    # Get name of the current function. If not found, skip to the next element.
    if (is.name(fn)) {
      fn_name <- as.character(fn)
    } else if (is.call(fn)) {
      fn_name <- deparse(fn)[1]
    } else {
      next
    }

    # Remove any package prefix (e.g., "pkg::fn" --> "fn")
    name_bare <- sub("^.*::", "", fn_name)

    # When an exported function is found, return the environment of its frame
    if (name_bare %in% exports) {
      return(frames[[i]])
    }
  }

  cli::cli_warn("No exported function found on the call stack.")
  environment()
}
