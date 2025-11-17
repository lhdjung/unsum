#' @include fn-formals.R generate.R plot-basic.R horns-plot.R

formals_final <- list(
  # Get the list of formals arguments from `generate_from_mean_sd_n()`, then
  # reduce it to the subset of arguments that exported sample generator
  # functions like `closure_generate()` should have.
  generator = generate_from_mean_sd_n |>
    formals() |>
    formals_remove(
      "technique",
      "rounding_error_mean",
      "rounding_error_sd"
    ),
  # Same for bar plot functions
  plot_fn_freq_bar = plot_frequency_bar |>
    formals() |>
    formals_remove(
      "technique",
      "facet_labels",
      "facet_labels_parens",
      "frequency_rows_subset",
      "min_max_values"
    ),
  # Same for horns index distribution plot functions
  plot_fn_horns_freq = plot_horns_frequency |>
    formals() |>
    formals_remove("type", "technique")
)


# Build helper that constructs functions like `closure_generate()`. The output
# function is just a wrapper around `generate_from_mean_sd_n()` that passes
# user-supplied arguments to the formal arguments -- which, in turn, are simply
# the `formals_generator_final` list. The only real exception is `technique`:
# the outer function takes a `technique` string like "CLOSURE" and inserts it to
# specify the `technique` argument of `generate_from_mean_sd_n()`.
new_generator_mean_sd_n <- function(technique) {
  rlang::new_function(
    args = formals_final$generator,
    body = rlang::expr({
      generate_from_mean_sd_n(
        mean = mean,
        sd = sd,
        n = n,
        scale_min = scale_min,
        scale_max = scale_max,
        technique = !!technique,
        path = path,
        stop_after = stop_after,
        include = include,
        rounding = rounding,
        threshold = threshold,
        ask_to_proceed = ask_to_proceed,
        rounding_error_mean = NULL,
        rounding_error_sd = NULL
      )
    })
  )
}


# Build helper that constructs basic barplot functions like
# `closure_plot_bar()`. The second argument, `bar_color`, will become the
# default for the output function's `bar_color` argument.
new_plot_fn_bar <- function(technique, bar_color) {
  rlang::new_function(
    # Changing a default here, not above, because each technique needs its own
    # color, and this color is only known from the call to `new_plot_fn_bar()`
    args = formals_final$plot_fn_freq_bar |>
      formals_change_defaults(bar_color = bar_color),

    body = rlang::expr({
      plot_frequency_bar(
        data = data,
        technique = !!technique,
        frequency = frequency,
        samples = samples,
        bar_alpha = bar_alpha,
        bar_color = bar_color,
        show_text = show_text,
        text_color = text_color,
        text_size = text_size,
        text_offset = text_offset,
        mark_thousand = mark_thousand,
        mark_decimal = mark_decimal,
        frequency_rows_subset = "all"
      )
    })
  )
}


# Build helper that constructs horns frequency plot functions like
# `closure_plot_horns_histogram()` as well as `closure_plot_horns_density()`.
# The second argument, `type`, makes the difference between these two functions.
# It has to be either "histogram" or "density". For each of these two types,
# there is an argument that it doesn't need (but that the other type does need).
# This argument is determined first and removed below.
new_plot_fn_horns_frequency <- function(technique, type) {
  arg_not_needed <- switch(
    type,
    "histogram" = "density_limits",
    "density" = "binwidth",
    cli::cli_abort("Internal error: invalid `type` value \"{type}\".")
  )

  rlang::new_function(
    args = formals_final$plot_fn_horns_freq |>
      formals_remove(arg_not_needed),

    body = rlang::expr({
      plot_horns_frequency(
        data = data,
        technique = !!technique,
        type = !!type,
        alpha = alpha,
        color = color,
        binwidth = binwidth,
        line_color_min_max = line_color_min_max,
        line_color_uniform = line_color_uniform,
        text_limits = text_limits,
        text_size = text_size,
        mark_decimal = mark_decimal
      )
    })
  )
}
