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

    # Generator ---------------------------------------------------------------

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
      `f_expected`, are doubles instead of integers. This is because doubles are
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
        - `value`: double. Scale values derived from `scale_min` and
        `scale_max`.
        - `f_expected`: double. Average count of each scale value across the
        group's samples.
        - `f_representative`: double. Count of each scale value in the group's
        medoid, i.e., the actual sample with the smallest total distance (EMD)
        to all others. `NaN` if `path` was specified because the medoid can't
        be found while streaming results to disk.
        - `f_relative`: double. `f_expected` divided by `n`.
      - **`modality_counts`**, **`modality_pairs`**, **`modality_shapes`**,
      **`modality_summary`**, **`modality_prominence`**: the range of counts
      of each scale value, and the shapes (e.g., one or two modes) that the
      samples take.
      - **`modality_conclusion`**: whether any sample can be unimodal,
      bimodal, or J-shaped. `NA` means that no such sample was found, but the
      search was partial, so it might still exist. This is always the case with
      SPRITE, and with CLOSURE if `stop_after` was specified.
      - **`results`**:
        - `id`: double. Runs from `1` to `samples_all`.
        - `v1`, `v2`, etc. (not present by default if `path` was specified):
        integer. One column per scale value, holding the number of times this
        value occurs in the sample. For instance, `v3` counts the 3s. Negative
        values are marked with `n`, as in `vn2` for -2, and decimal values with
        `_`, as in `v1_5` for 1.5 (only for SPRITE with `items > 1`). Together,
        these columns describe each sample completely because the order of
        values within a sample doesn't matter. The counts of each row sum up to
        `n`.
        - `horns`: double. Horns index of each sample.
      - **`directory`** (only present if `path` was specified):
        - `path`: string. Location of the folder in which the results were
        saved."
    ),

    # Downstream functions (any) ----------------------------------------------

    # `data` parameter of functions that are downstream from a generator
    param_data = if (technique == "CLOSURE") {
      glue::glue(
        "List returned by [`{lowtech}_generate()`] or [`{lowtech}_read()`]."
      )
    } else {
      glue::glue("List returned by [`{lowtech}_generate()`].")
    },

    # Readers and writers -----------------------------------------------------

    read_write_description = glue::glue(
      "You can use `{lowtech}_write()` to save the results of
      [`{lowtech}_generate()`] on your computer. A message will show the exact
      location.

      The data are saved in a new folder as separate Parquet files, one for
      each tibble in `{lowtech}_generate()`'s output. `results` is saved as
      counts.parquet, along with scale_values.parquet which maps the count
      columns to scale values.

      `{lowtech}_read()` is the opposite: it reads those files back into R,
      recreating the original {technique} list. This is useful for later
      analyses if you don't want to re-run a lengthy `{lowtech}_generate()`
      call. It also works with results that `{lowtech}_generate()` wrote
      to disk itself using `path = \"your/path\"`."
    ),

    read_write_param_path = glue::glue(
      "String (length 1). File path where `{lowtech}_write()` will create a
      new folder with the results. Set it to `\".\"` to choose the current
      working directory. For `{lowtech}_read()`, the path to an existing
      folder with results."
    ),

    read_write_param_include = glue::glue(
      "String (length 1). Which parts of the detailed results should be read in?
      - With `\"stats_only\"`, the default, no results are read.
      - `\"stats_and_horns\"` reads the horns index values, but not the samples.
      - `\"capped_error\"` checks whether the number of samples is higher than a
      given threshold (see `samples_cap`). If so, it throws an error;
      but if not, it reads both the samples and the horns values.
      - `\"all\"` reads both the samples and the horns values."
    ),

    read_write_folder_name = glue::glue(
      "Folder name: The new folder's name will contain all the inputs that
      determine the {technique} results. Dashes separate values and
      underscores replace decimal periods. For example:

      \\preformatted{{

      {technique}-3_5-1_0-90-1-5-up_or_down-5
      }}

      The order is the same as in `{lowtech}_generate()`:

      \\preformatted{{

      {lowtech}_generate(
        mean = \"3.5\",
        sd = \"1.0\",
        n = 90,
        scale_min = 1,
        scale_max = 5,
        rounding = \"up_or_down\",  # default
        threshold = 5             # default
      )

     }}"
    ),

    read_write_details = glue::glue(
      "`{lowtech}_write()` saves all tibbles as Parquet files. This is much
      faster and takes up far less disk space --- roughly 1% of a CSV file
      with the same data. Speed and disk space can be relevant with large
      result sets.

      Use `{lowtech}_read()` to import the {technique} list from the folder
      back into R. This is based on [`nanoparquet::read_parquet()`]."
    ),

    read_write_return = glue::glue(
      "- `{lowtech}_write()` returns the path to the new folder it created.
      - `{lowtech}_read()` returns a list of the same kind as
      [`{lowtech}_generate()`]."
    ),

    # Plot functions ----------------------------------------------------------

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

    if (appendix == "") {
      return(out)
    }

    out |>
      paste(appendix) |>
      add_class("glue")
  } else {
    out
  }
}
