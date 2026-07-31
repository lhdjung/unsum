# Create a wrapper around an existing implementation function. By default, the
# wrapper will forward all arguments to the implementation and have the same
# defaults. This can be adjusted via `args_defaults`. You can also hardcode
# argument specifications in the forwarding call via `args_hardcode`. If you do,
# they won't be arguments of the wrapper. If there are dots `...`, they are
# forwarded by default, but you can use `pass_dots = FALSE` so the wrapper won't
# support the dots.
new_wrapper <- function(
  impl_fn,
  args_defaults = list(),
  args_hardcode = list(),
  pass_dots = TRUE
) {
  fmls <- formals(impl_fn)
  names_fmls <- names(fmls)

  args_hardcode_expr <- rlang::enexpr(args_hardcode)

  if (rlang::is_call(args_hardcode_expr, "list")) {
    # Literal `list()` call: keep elements unevaluated, so symbols like `mean`
    # appear by name in the generated call body rather than as inlined objects
    args_hardcode <- args_hardcode_expr[names(args_hardcode_expr) != ""]
  } else {
    # Pre-built variable: evaluate eagerly in the caller env
    args_hardcode <- rlang::eval_bare(args_hardcode_expr, rlang::caller_env())
    if (length(args_hardcode) > 0 && !rlang::is_named(args_hardcode)) {
      rlang::abort("`args_hardcode` must be a fully named list.")
    }
  }

  names_defaults <- names(args_defaults)
  names_hardcode <- names(args_hardcode)

  # Validate `args_hardcode`
  if (length(args_hardcode) > 0) {
    if (!all(names_hardcode %in% names_fmls)) {
      offenders <- names_hardcode[!names_hardcode %in% names_fmls]
      rlang::abort(c(
        "All `args_hardcode` items must refer to existing arguments.",
        x = paste("Non-existing arguments:", offenders)
      ))
    } else if (any(names_hardcode == "...")) {
      rlang::abort("Can't use \"...\" as an argument name in `args_hardcode`.")
    }
  }

  # Validate `args_defaults` and apply defaults to list of arguments
  if (length(args_defaults) > 0) {
    if (!all(names_defaults %in% names_fmls)) {
      offenders <- names_defaults[!names_defaults %in% names_fmls]
      rlang::abort(c(
        "All `args_defaults` items must refer to existing arguments.",
        x = paste("Non-existing arguments:", offenders)
      ))
    }

    offenders <- names_hardcode[names_hardcode %in% names_defaults]
    if (length(offenders) > 0) {
      rlang::abort(c(
        "Can't provide defaults for hardcoded arguments.",
        x = paste(
          "These are in both `args_defaults` and `args_hardcode`:",
          offenders
        )
      ))
    }

    # Insert user-specified new defaults into the list of formal arguments
    for (i in seq_along(args_defaults)) {
      fmls[names_fmls == names_defaults[i]] <- args_defaults[[i]]
    }
  }

  # Dots can't be used like `... = value`
  names_call_args <- setdiff(names_fmls, "...")

  call_args <- lapply(names_call_args, function(name) {
    if (name %in% names_hardcode) {
      args_hardcode[[name]]
    } else {
      rlang::sym(name)
    }
  })

  names(call_args) <- names_call_args

  # Use dots by default
  if (pass_dots && "..." %in% names_fmls) {
    call_args <- c(call_args, list(quote(...)))
  }

  # The wrapper can't have arguments that were hardcoded in the body. The dots
  # can optionally be excluded.
  exclude <- if (pass_dots) names_hardcode else c(names_hardcode, "...")

  # Execution environment for the wrapper where `impl_fn`
  wrapper_env <- rlang::new_environment(
    list(.impl = impl_fn),
    parent = parent.frame()
  )

  # Clean up local objects after factory finishes, else they'd linger in its env
  on.exit(rm(list = rlang::env_names(rlang::current_env())))

  # Create the wrapper
  rlang::new_function(
    args = as.pairlist(fmls[!names_fmls %in% exclude]),
    body = rlang::call2(".impl", !!!call_args),
    env = wrapper_env
  )
}
