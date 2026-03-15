#' @include utils.R

# Package-level flag set to TRUE during internal S7 object construction.
# S7 calls property setters during construction, so new_read_only_setter()
# checks this flag to allow the initial set while blocking later user changes.
.s7_constructing <- new.env(parent = emptyenv())
.s7_constructing$active <- FALSE


# Factory that creates a setter for a named property. The setter:
#  - During construction (.s7_constructing$active == TRUE): sets the value via
#    `attr<-()` (bypasses the setter, preventing infinite recursion) and returns self.
#  - After construction: aborts with a read-only error.
new_read_only_setter <- function(prop_name) {
  force(prop_name)

  function(self, value) {
    if (.s7_constructing$active) {
      # Read: the property of self with this name gets that value, then return
      return(`attr<-`(self, prop_name, value))
    }
    cli::cli_abort(c(
      "Result lists are read-only.",
      "x" = "Their elements, like {.field {prop_name}}, cannot be assigned
      new values.",
      "i" = "Use a {.fn *_generate} function to create a new result list."
    ))
  }
}


# fmt: skip
ResultListFromMeanSdN <- S7::new_class(
  "ResultListFromMeanSdN",
  abstract = TRUE,
  properties = list(
    inputs            = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("inputs")),
    metrics_main      = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("metrics_main")),
    metrics_horns     = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("metrics_horns")),
    modality_analysis = S7::new_property(S7::class_list,       setter = new_read_only_setter("modality_analysis")),
    frequency         = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("frequency")),
    frequency_dist    = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("frequency_dist")),
    results           = S7::new_property(S7::class_data.frame, setter = new_read_only_setter("results"))
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


# Enable access to list elements via `$` which S7 does not support by default
S7::method(`$`, ResultListFromMeanSdN) <- function(x, name) {
  S7::prop(x, name)
}

# Same with subsetting like `x[name]` below
S7::method(`[`, ResultListFromMeanSdN) <- function(x, name) {
  S7::prop(x, name)
}


# Print -------------------------------------------------------------------

# Print method for CLOSURE and SPRITE results
S7::method(print, ResultListFromMeanSdN) <- function(x, ...) {
  technique <- x@inputs$technique[[1L]]
  samples_all <- x@metrics_main$samples_all

  format_comma <- scales::label_comma()
  msg_samples_all <- format_comma(samples_all)
  msg_sample_s <- if (samples_all == 1L) "sample" else "samples"

  has_directory <- has_property(data, "directory")

  cli::cli_h1(
    "{.bold {technique} results: {msg_samples_all} {msg_sample_s}}"
  )

  visible <- c(
    # fmt: skip
    inputs = paste0(
      "how `", tolower(technique), "_generate()` produced these data"
    ),
    metrics_main = "key statistics about the results",
    metrics_horns = "on the distribution of horns index values",
    directory = if (has_directory) {
      "where the results are saved on disk"
    }
  )

  # Print each visible tibble with a header like `$inputs` that emphasizes its
  # name within the result list. It is followed by a brief description. These
  # two elements are separated by a middle dot rendered via `\u00B7`.
  for (name in names(visible)) {
    cat("\n")
    cli::cli_text("{.field ${name}} \u00B7 {visible[name]}")
    print(x[name])
  }

  cat("\n")

  hidden <- list(
    frequency = paste0("full frequency table (", nrow(x@frequency), " rows)"),
    frequency_dist = paste0(
      "per-value count distributions (",
      format_comma(nrow(x@frequency_dist)),
      " rows)"
    ),
    modality_analysis = "ordering and modality analysis (3 tibbles)",
    results = paste0(
      "all samples and their horns indices (",
      msg_samples_all,
      " rows)"
    )
  )

  message("With hidden elements:")

  for (name in names(hidden)) {
    cli::cli_alert_info("Access {.field ${name}} for {hidden[name]}")
  }

  invisible(x)
}
