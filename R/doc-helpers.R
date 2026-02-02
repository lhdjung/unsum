closure_to_sprite <- function(string) {
  string |>
    gsub("CLOSURE", "SPRITE", x = _) |>
    gsub("closure", "sprite", x = _)
}


# Call within roxygen2 docs as inline R code to generate sections of the
# documentation for specific techniques, such as CLOSURE or SPRITE
expand_section <- function(section, technique) {
  lowtech <- tolower(technique)

  out <- switch(
    section,

    # Three sections in the docs of generators, i.e., `closure_generate()` etc.
    writing = glue::glue(
      "Writing to disk: Specify `path` if the expected runtime is very
      long. (In case you have trouble choosing a path, use `path = \".\"` for
      your current working directory.) This makes sure the results are preserved
      by incrementally writing them to disk. Otherwise, you might encounter an
      out-of-memory error because `{lowtech}_generate()` accumulates more data
      than your computer can hold in memory."
    ),
    memory = glue::glue(
      "More about memory: Some output columns that contain counts, such as
      `f_absolute`, are doubles instead of integers. This is because doubles are
      able to contain much larger numbers. When counting {technique} results, it
      is possible to exceed the limit of 32-bit integers in R, which is roughly
      two billion."
    ),
    rounding = glue::glue(
      "Rounding limitations: The `rounding` and `threshold` arguments are not
      fully implemented. For example, {technique} currently treats all rounding
      bounds as inclusive, even if the `rounding` value would imply otherwise.
      Many specifications of the two arguments will not make any difference,
      and those that do will most likely lead to empty results."
    ),

    # Return section for generators
    generate_return = glue::glue(
      "`{lowtech}_generate()` returns a named list of tibbles (data frames):
      - **`inputs`**: Arguments to this function.
      - **`metrics_main`**:
        - `samples_all`: double. Number of all samples. Equal to the number of
        rows in `results`.
        - `values_all`: double. Number of all individual values found. Equal to
        `n * samples_all`.
      - **`metrics_horns`**:
        - `mean`: double. Average horns value of all samples. The horns index is
        a measure of dispersion for bounded scales; see [`horns()`].
        - `uniform`: double. The value that `mean` would have if all samples
        were uniformly distributed; see [`horns_uniform()`].
        - `sd`, `cv`, `mad`, `min`, `median`, `max`, `range`: double. Standard
        deviation, coefficient of variation, median absolute deviation, minimum,
        median, maximum, and range of the horns index values across all samples.
        Note that `mad` is not scaled using a constant, as [`stats::mad()`] is
        by default.
      - **`frequency`**:
        - `samples`: string. Frequencies apply to one of three subsets of
        samples: `\"all\"` for all samples, `\"horns_min\"` for those samples
        with the lowest horns index among all samples, and `\"horns_max\"` for
        those samples with the highest horns index.
        - `value`: integer. Scale values derived from `scale_min` and
        `scale_max`.
        - `f_average`: double. Count of scale values in the mean `results`
        sample.
        - `f_absolute`: double. Count of individual scale values found in the
        `results` samples.
        - `f_relative`: double. Values' share of total values found.
      - **`results`**:
        - `id`: integer. Runs from `1` to `samples_all`.
        - `sample` (not present by default if `path` was specified): list of
        integer vectors. Each of these vectors has length `n`. It is a sample
        (or distribution) of individual scale values found by {technique}.
        - `horns`: double. Horns index of each sample.
      - **`directory`** (only present if `path` was specified):
        - `path`: string. Location of the folder in which the results were
        saved."
    ),

    # `data` parameter of functions that are downstream from a generator
    param_data = if (technique == "CLOSURE") {
      glue::glue(
        "List returned by [`{lowtech}_generate()`] or [`{lowtech}_read()`]."
      )
    } else {
      glue::glue("List returned by [`{lowtech}_generate()`].")
    },

    # Description section of bar plot functions, i.e., `closure_plot_bar()` etc.
    plot_bar_description = glue::glue(
      "Call `{lowtech}_plot_bar()` to get a barplot of {technique} results.

      For each scale value, the bars show how often this value appears in the
      mean samples with the minimum or maximum horns index (\\eqn{{h}}).
      This displays the typical sample with the least or most amount
      of variance from among all {technique} samples."
    ),

    cli::cli_abort("Internal error: Invalid `section` specification.")
  )

  # Some statements in the docs only apply to CLOSURE
  if (technique == "CLOSURE") {
    appendix <- switch(
      section,

      memory = "For instance, this will happen with `values_all` in the examples
      (`data_high`) if `n` is changed to `120` and `scale_max` to `7`.",

      plot_bar_description = "\n\nAs CLOSURE finds all possible samples,
      this plot shows the min and max *possible* amount of variance
      given the input statistics.",

      ""
    )

    return(paste(out, appendix))
  }

  out
}
