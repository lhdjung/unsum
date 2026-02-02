#' @include fn-formals.R generate-basic.R plot-bar-basic.R
#' @include plot-horns-histogram-basic.R

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
    formals_remove("technique"),

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
        rounding_error_sd = NULL,
        items = items
      )
    })
  )
}


# Build helper that constructs basic barplot functions like
# `closure_plot_bar()`. The second argument, `bar_color`, will become the
# default for the output function's `bar_color` argument.
new_plot_fn_bar <- function(technique, bar_color) {
  args_all <- formals_final$plot_fn_freq_bar |>
    formals_add_defaults(bar_color = bar_color)

  # DEMO functions are based on frequencies, not CLOSURE-type result lists.
  # Therefore, replace the `data` argument by one called `freqs`, and remove
  # arguments that only apply to CLOSURE-type techniques.
  if (technique == "DEMO") {
    args_all <- args_all |>
      formals_add("freqs", .after = "data") |>
      formals_remove(
        "data",
        "min_max",
        "samples",
        "facet_labels",
        "facet_labels_parens"
      )

    data_arg <- rlang::expr(freqs)
    samples_arg <- NULL
    min_max_arg <- NULL
    facet_labels_arg <- NULL
    facet_labels_parens_arg <- NULL
  } else {
    data_arg <- rlang::expr(data)
    samples_arg <- rlang::expr(samples)
    min_max_arg <- rlang::expr(min_max)
    facet_labels_arg <- rlang::expr(facet_labels)
    facet_labels_parens_arg <- rlang::expr(facet_labels_parens)
  }

  rlang::new_function(
    args = args_all,
    body = rlang::expr({
      plot_frequency_bar(
        data = !!data_arg,
        technique = !!technique,
        min_max = !!min_max_arg,
        format = format,
        samples = !!samples_arg,
        facet_labels = !!facet_labels_arg,
        facet_labels_parens = !!facet_labels_parens_arg,
        bar_alpha = bar_alpha,
        bar_color = bar_color,
        show_text = show_text,
        text_color = text_color,
        text_size = text_size,
        text_offset = text_offset,
        mark_thousand = mark_thousand,
        mark_decimal = mark_decimal
      )
    })
  )
}


# Build helper that constructs horns frequency plot functions like
# `closure_plot_horns_histogram()`. The `bar_color` argument works as above.
new_plot_fn_horns_frequency <- function(technique, bar_color) {
  rlang::new_function(
    args = formals_final$plot_fn_horns_freq |>
      formals_add_defaults(bar_color = bar_color),

    body = rlang::expr({
      plot_horns_frequency(
        data = data,
        technique = !!technique,
        bar_color = bar_color,
        alpha = alpha,
        binwidth = binwidth,
        show_labels = show_labels,
        line_color_min_max = line_color_min_max,
        line_color_reference = line_color_reference,
        text_limits = text_limits,
        text_size = text_size,
        mark_thousand = mark_thousand,
        mark_decimal = mark_decimal
      )
    })
  )
}
