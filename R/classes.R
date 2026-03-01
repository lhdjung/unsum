#' @include utils.R
NULL


# Helper ------------------------------------------------------------------

# Validate a tibble's dimensions and column names/types. Returns NULL if the
# tibble is valid, or a character string describing the problem if not. This
# is the format S7 validators require: NULL for success, string for failure.
validate_tibble_structure <- function(x, name, dims, col_names_types) {
  if (!inherits(x, "tbl_df")) {
    return(paste0("`", name, "` must be a tibble (tbl_df)."))
  }

  if (nrow(x) != dims[1] || ncol(x) != dims[2]) {
    return(paste0(
      "`", name, "` must have ", dims[1], " row(s) and ", dims[2], " column(s),",
      " but has ", nrow(x), " and ", ncol(x), "."
    ))
  }

  if (!identical(names(x), names(col_names_types))) {
    return(paste0(
      "`", name, "` must have column names: ",
      paste(names(col_names_types), collapse = ", "), "."
    ))
  }

  type_ok <- mapply(
    function(actual, expected) any(actual == expected),
    vapply(x, typeof, character(1)),
    col_names_types
  )

  if (!all(type_ok)) {
    bad <- names(col_names_types)[!type_ok]
    return(paste0(
      "Column(s) ", paste(bad, collapse = ", "), " in `", name, "` have wrong types."
    ))
  }

  NULL
}


# Abstract base classes ---------------------------------------------------

#' Abstract base class for all unsum generator/reader output
#'
#' `UnsumResult` is the root of the class hierarchy. All objects returned by
#' functions like [closure_generate()] and [closure_read()] are subclasses of
#' `UnsumResult`. Use `is(x, "UnsumResult")` to test for membership.
#'
#' @noRd
UnsumResult <- S7::new_class(
  "UnsumResult",
  properties = list(
    inputs        = S7::new_property(S7::class_any),
    metrics_main  = S7::new_property(S7::class_any),
    metrics_horns = S7::new_property(S7::class_any),
    frequency     = S7::new_property(S7::class_any)
  ),
  validator = function(self) {
    err <- validate_tibble_structure(
      self@inputs, "inputs", c(1L, 8L),
      list(
        technique = "character",
        mean      = "character",
        sd        = "character",
        n         = c("integer", "double"),
        scale_min = c("integer", "double"),
        scale_max = c("integer", "double"),
        rounding  = "character",
        threshold = c("integer", "double")
      )
    )
    if (!is.null(err)) return(err)

    err <- validate_tibble_structure(
      self@metrics_main, "metrics_main", c(1L, 2L),
      list(samples_all = "double", values_all = "double")
    )
    if (!is.null(err)) return(err)

    err <- validate_tibble_structure(
      self@metrics_horns, "metrics_horns", c(1L, 9L),
      list(
        mean    = "double",
        uniform = "double",
        sd      = "double",
        cv      = "double",
        mad     = "double",
        min     = "double",
        median  = "double",
        max     = "double",
        range   = "double"
      )
    )
    if (!is.null(err)) return(err)

    # The frequency table has 3 groups (all, horns_min, horns_max) of
    # scale_length rows each, for a total of 3 * scale_length rows. Empty
    # results are the exception: they have only one group ("all"), so only
    # scale_length rows.
    scale_length <- self@inputs$scale_max - self@inputs$scale_min + 1L
    n_freq_rows  <- nrow(self@frequency)

    if (!n_freq_rows %in% c(scale_length, 3L * scale_length)) {
      return(paste0(
        "`frequency` must have ", scale_length, " or ", 3L * scale_length,
        " rows, but has ", n_freq_rows, "."
      ))
    }

    err <- validate_tibble_structure(
      self@frequency, "frequency", c(n_freq_rows, 5L),
      list(
        samples    = "character",
        value      = "integer",
        f_average  = "double",
        f_absolute = "double",
        f_relative = "double"
      )
    )
    if (!is.null(err)) return(err)

    NULL
  },
  abstract = TRUE
)


# CLOSURE-specific abstract base class
ClosureResult <- S7::new_class(
  "ClosureResult",
  parent   = UnsumResult,
  abstract = TRUE,
  validator = function(self) {
    if (!identical(self@inputs$technique, "CLOSURE")) {
      "`inputs$technique` must be \"CLOSURE\"."
    }
  }
)


# SPRITE-specific abstract base class
SpriteResult <- S7::new_class(
  "SpriteResult",
  parent   = UnsumResult,
  abstract = TRUE,
  validator = function(self) {
    if (!identical(self@inputs$technique, "SPRITE")) {
      "`inputs$technique` must be \"SPRITE\"."
    }
  }
)


# In-memory CLOSURE results -----------------------------------------------

#' In-memory CLOSURE result
#'
#' Returned by [closure_generate()] when no `path` is specified. Contains all
#' four summary tibbles plus a `results` tibble with the full sample data.
#'
#' @noRd
ClosureResultFull <- S7::new_class(
  "ClosureResultFull",
  parent     = ClosureResult,
  properties = list(results = S7::new_property(S7::class_any)),
  validator  = function(self) {
    n_samples <- self@metrics_main$samples_all
    if (n_samples == 0) return(NULL)
    validate_tibble_structure(
      self@results, "results", c(n_samples, 3L),
      list(id = "double", sample = "list", horns = "double")
    )
  }
)


# CLOSURE results from disk -----------------------------------------------

# Abstract base: all CLOSURE results read back from a folder on disk
ClosureResultFromDisk <- S7::new_class(
  "ClosureResultFromDisk",
  parent     = ClosureResult,
  properties = list(directory = S7::new_property(S7::class_any)),
  abstract   = TRUE,
  validator  = function(self) {
    validate_tibble_structure(
      self@directory, "directory", c(1L, 1L),
      list(path = "character")
    )
  }
)


#' CLOSURE result with summary statistics only
#'
#' Returned by [closure_read()] with `include = "stats_only"`. Contains the
#' four summary tibbles and a `directory` tibble, but no `results`.
#'
#' @noRd
ClosureResultStatsOnly <- S7::new_class(
  "ClosureResultStatsOnly",
  parent = ClosureResultFromDisk
)


#' CLOSURE result with statistics and horns index values
#'
#' Returned by [closure_read()] with `include = "stats_and_horns"`. Contains
#' the four summary tibbles, a `directory` tibble, and a `results` tibble with
#' columns `id` and `horns` (no raw samples).
#'
#' @noRd
ClosureResultStatsAndHorns <- S7::new_class(
  "ClosureResultStatsAndHorns",
  parent     = ClosureResultFromDisk,
  properties = list(results = S7::new_property(S7::class_any)),
  validator  = function(self) {
    n_samples <- self@metrics_main$samples_all
    if (n_samples == 0) return(NULL)
    validate_tibble_structure(
      self@results, "results", c(n_samples, 2L),
      list(id = "double", horns = "double")
    )
  }
)


#' Full CLOSURE result from disk
#'
#' Returned by [closure_read()] with `include = "all"` or
#' `include = "capped_error"`. Contains all summary tibbles, a `directory`
#' tibble, and a complete `results` tibble with columns `id`, `sample`, and
#' `horns`.
#'
#' @noRd
ClosureResultAll <- S7::new_class(
  "ClosureResultAll",
  parent     = ClosureResultFromDisk,
  properties = list(results = S7::new_property(S7::class_any)),
  validator  = function(self) {
    n_samples <- self@metrics_main$samples_all
    if (n_samples == 0) return(NULL)
    validate_tibble_structure(
      self@results, "results", c(n_samples, 3L),
      list(id = "double", sample = "list", horns = "double")
    )
  }
)


# In-memory SPRITE results ------------------------------------------------

#' In-memory SPRITE result
#'
#' Returned by [sprite_generate()] when no `path` is specified.
#'
#' @noRd
SpriteResultFull <- S7::new_class(
  "SpriteResultFull",
  parent     = SpriteResult,
  properties = list(results = S7::new_property(S7::class_any)),
  validator  = function(self) {
    n_samples <- self@metrics_main$samples_all
    if (n_samples == 0) return(NULL)
    validate_tibble_structure(
      self@results, "results", c(n_samples, 3L),
      list(id = "double", sample = "list", horns = "double")
    )
  }
)


# SPRITE results from disk ------------------------------------------------

# Abstract base: all SPRITE results read back from a folder on disk
SpriteResultFromDisk <- S7::new_class(
  "SpriteResultFromDisk",
  parent     = SpriteResult,
  properties = list(directory = S7::new_property(S7::class_any)),
  abstract   = TRUE,
  validator  = function(self) {
    validate_tibble_structure(
      self@directory, "directory", c(1L, 1L),
      list(path = "character")
    )
  }
)


#' SPRITE result with summary statistics only
#'
#' Returned by [sprite_read()] with `include = "stats_only"`.
#'
#' @noRd
SpriteResultStatsOnly <- S7::new_class(
  "SpriteResultStatsOnly",
  parent = SpriteResultFromDisk
)


#' SPRITE result with statistics and horns index values
#'
#' Returned by [sprite_read()] with `include = "stats_and_horns"`.
#'
#' @noRd
SpriteResultStatsAndHorns <- S7::new_class(
  "SpriteResultStatsAndHorns",
  parent     = SpriteResultFromDisk,
  properties = list(results = S7::new_property(S7::class_any)),
  validator  = function(self) {
    n_samples <- self@metrics_main$samples_all
    if (n_samples == 0) return(NULL)
    validate_tibble_structure(
      self@results, "results", c(n_samples, 2L),
      list(id = "double", horns = "double")
    )
  }
)


#' Full SPRITE result from disk
#'
#' Returned by [sprite_read()] with `include = "all"` or
#' `include = "capped_error"`.
#'
#' @noRd
SpriteResultAll <- S7::new_class(
  "SpriteResultAll",
  parent     = SpriteResultFromDisk,
  properties = list(results = S7::new_property(S7::class_any)),
  validator  = function(self) {
    n_samples <- self@metrics_main$samples_all
    if (n_samples == 0) return(NULL)
    validate_tibble_structure(
      self@results, "results", c(n_samples, 3L),
      list(id = "double", sample = "list", horns = "double")
    )
  }
)
