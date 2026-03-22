#' @include utils.R

# Package-level flag set to `TRUE` during internal S7 object construction. S7
# calls property setters during construction, so `new_read_only_setter()` checks
# this flag to allow the initial set while blocking later user changes.
.s7_constructing <- new.env(parent = emptyenv())
.s7_constructing$active <- FALSE


# Error-throwing helper that runs in case the user tries to overwrite existing
# fields of an S7 object. Defined separately from `new_read_only_setter()` so
# the code is not copied for each factory-made function.
abort_read_only <- function(x, prop_name) {
  if (S7::S7_inherits(x, ResultListFromMeanSdN)) {
    technique <- x@inputs$technique
    lowtech <- tolower(technique)
    msg_generate <- c(
      "i" = "Use {.fn {lowtech}_generate} to create a new result list."
    )

    cli::cli_abort(
      c(
        "{technique} results are read-only.",
        x = "Their elements, such as {.field {prop_name}}, cannot be assigned
        new values.",
        msg_generate
      ),
      # Prevent a confusing mention of the current function in the error message
      call = rlang::caller_env()
    )
  }

  # Fallthrough error for possible future extensions
  cli::cli_abort(
    c(
      "This object is read-only.",
      x = "Its elements, like like {.field {prop_name}}, cannot be assigned
      new values."
    ),
    call = rlang::caller_env()
  )
}


# Function factory that creates a setter for a named property:
#  -- During construction (`.s7_constructing$active == TRUE`), the setter sets
# the value via `attr<-()`, and then returns `self`.
#  -- After construction: aborts with an error that says "read-only" etc.
new_read_only_setter <- function(prop_name) {
  force(prop_name)

  function(self, value) {
    if (.s7_constructing$active) {
      # Read: the property of self with this name gets that value, then return
      `attr<-`(self, prop_name, value)
    } else {
      abort_read_only(self, prop_name)
    }
  }
}

# fmt: skip
ResultListFromMeanSdN <- S7::new_class(
  "ResultListFromMeanSdN",
  abstract = TRUE,
  properties = list(
    inputs              = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("inputs")),
    metrics_main        = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("metrics_main")),
    metrics_horns       = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("metrics_horns")),
    modality_counts     = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("modality_counts")),
    modality_pairs      = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("modality_pairs")),
    modality_conclusion = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("modality_conclusion")),
    frequency           = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("frequency")),
    frequency_dist      = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("frequency_dist")),
    results             = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("results"))
  )
)


ClosureResult <- S7::new_class(
  "ClosureResult",
  parent = ResultListFromMeanSdN,
  validator = function(self) {
    check_generator_output(self, "CLOSURE", allow_empty = TRUE)
  }
)

SpriteResult <- S7::new_class(
  "SpriteResult",
  parent = ResultListFromMeanSdN,
  validator = function(self) {
    check_generator_output(self, "SPRITE", allow_empty = TRUE)
  }
)


# Accessors ---------------------------------------------------------------

# To ensure immutability, enable throwing the bespoke error from the "read-only
# setter" when the user attempts to subassign elements of a result list
S7::method(`$<-`, ResultListFromMeanSdN) <- function(x, name, value) {
  new_read_only_setter(name)(x)
}

# Allow access to list elements via `$` which S7 does not support by default
S7::method(`$`, ResultListFromMeanSdN) <- function(x, name) {
  S7::prop(x, name)
}

# Same with subsetting like `x[name]` below, except the tibble's name is
# retained, as in vector subsetting
S7::method(`[`, ResultListFromMeanSdN) <- function(x, name) {
  `names<-`(list(S7::prop(x, name)), name)
}

# Same for `x[[name]]` so the name is *not* retained; again like with vectors
S7::method(`[[`, ResultListFromMeanSdN) <- function(x, name) {
  S7::prop(x, name)
}

# When the user tries to subassign `x[name] <- new_value`, throw a more accurate
# and informative error than "S7 objects are not subsettable"
S7::method(`[<-`, ResultListFromMeanSdN) <- function(x, name, value) {
  new_read_only_setter(name)(x)
}

# Same but for `x[[name]] <- new_value`
S7::method(`[[<-`, ResultListFromMeanSdN) <- function(x, name, value) {
  new_read_only_setter(name)(x)
}

# Allow the user to get the length, i.e., the number of elements
S7::method(length, ResultListFromMeanSdN) <- function(x) {
  length(ResultListFromMeanSdN@properties)
}

# Allow the user to get the names of the elements
S7::method(names, ResultListFromMeanSdN) <- function(x) {
  names(ResultListFromMeanSdN@properties)
}

# Allow the user to convert the results object to an actual list. This is based
# on the `length()` and `names()` methods from above.
S7::method(as.list, ResultListFromMeanSdN) <- function(x) {
  out <- vector("list", length(x))
  names_all <- names(x)

  for (i in seq_along(out)) {
    out[[i]] <- x[[names_all[i]]]
  }

  `names<-`(out, names_all)
}


# Print -------------------------------------------------------------------

# Print method for CLOSURE and SPRITE results
S7::method(print, ResultListFromMeanSdN) <- function(
  x,
  show = c("some", "all", "none"),
  ...
) {
  show <- rlang::arg_match(show)

  technique <- x@inputs$technique[[1L]]
  samples_all <- x@metrics_main$samples_all

  format_comma <- scales::label_comma()
  msg_samples_all <- format_comma(samples_all)
  msg_sample_s <- if (samples_all == 1L) "sample" else "samples"

  has_directory <- has_property(x, "directory")

  cli::cli_h1(
    "{.bold {technique} results: {msg_samples_all} {msg_sample_s}}"
  )

  # Elements that are shown by default
  visible <- c(
    # fmt: skip
    inputs = paste0(
      "how `", tolower(technique), "_generate()` produced these data"
    ),
    metrics_main = "key statistics about the generated samples",
    metrics_horns = "on the distribution of horns index values",
    directory = if (has_directory) {
      "where the results are saved on disk"
    }
  )

  # Elements that are not shown by default
  hidden <- c(
    modality_counts = paste0(
      "min/max counts per scale value (",
      nrow(x@modality_counts),
      " rows)"
    ),
    modality_pairs = paste0(
      "frequency ordering between adjacent values (",
      nrow(x@modality_pairs),
      " rows)"
    ),
    modality_conclusion = "unimodal/J-shape flags (1 row)",
    frequency = paste0("full frequency table (", nrow(x@frequency), " rows)"),
    frequency_dist = paste0(
      "per-value count distributions (",
      format_comma(nrow(x@frequency_dist)),
      " rows)"
    ),
    results = paste0(
      "all samples and their horns indices (",
      msg_samples_all,
      " rows)"
    )
  )

  # The optional `show` argument in the explicit print method, like
  # `print(my_closure_results, show = "all")`, controls elements' visibility
  switch(
    show,
    "all" = {
      visible <- c(visible, hidden)
    },
    "none" = {
      hidden <- c(visible, hidden)
      visible <- NULL
    }
  )

  # Print each visible tibble with a header like `$inputs` that emphasizes its
  # name within the result list. It is followed by a brief description. These
  # two elements are separated by a middle dot rendered via `\u00B7`.
  for (name in names(visible)) {
    cat("\n")
    cli::cli_text("{.field ${name}} \u00B7 {visible[name]}")
    print(x[[name]])
  }

  cat("\n")

  msg_some <- "{.emph # Use `print(show = \"some\")` to see a few elements}"
  msg_none <- "{.emph # Use `print(show = \"none\")` to hide elements}"

  # All "show" variants other than "all" have some elements to hide
  if (show == "all") {
    cli::cli_inform(msg_some)
    cli::cli_inform(msg_none)
  } else {
    message("With hidden elements:")

    for (name in names(hidden)) {
      cli::cli_alert_info("Access {.field ${name}} for {hidden[name]}")
    }

    cat("\n")
    cli::cli_inform("{.emph # Use `print(show = \"all\")` to see all elements}")
    cli::cli_inform(switch(show, "some" = msg_none, "none" = msg_some))
  }

  cat("\n")

  invisible(x)
}
