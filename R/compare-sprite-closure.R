#' @include generate.R

# Arguments will be added below the function definition
compare_sprite_closure <- function() {
  check_single(iterations, c("double", "numeric"))

  args_all <- list(
    mean = mean,
    sd = sd,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    path = path,
    stop_after = stop_after,
    include = include,
    rounding = rounding,
    threshold = threshold,
    ask_to_proceed = ask_to_proceed,
    items = items
  )

  time_start <- proc.time()["elapsed"]
  data_sprite <- rlang::inject(sprite_generate(!!!args_all))
  time_end <- proc.time()["elapsed"]

  seconds_sprite <- time_end - time_start

  time_start <- proc.time()["elapsed"]
  data_closure <- rlang::inject(closure_generate(!!!args_all))
  time_end <- proc.time()["elapsed"]

  seconds_closure <- time_end - time_start

  time_ratio <- seconds_sprite / seconds_closure

  count_sprite <- data_sprite$metrics_main$samples_all
  count_closure <- data_closure$metrics_main$samples_all

  samples_ratio <- count_sprite / count_closure

  list(
    samples = tibble::tibble(
      count_sprite,
      count_closure,
      ratio = samples_ratio,
    ),
    time = tibble::tibble(
      seconds_sprite,
      seconds_closure,
      ratio = time_ratio,
    ),

    # TODO: COMBINE THESE TO ONE TIBBLE PER CATEGORY,
    # I.E., TWO TIBBLES BELOW IN TOTAL, INSTEAD OF FOUR
    metrics_horns_sprite = data_sprite$metrics_horns,
    metrics_horns_closure = data_closure$metrics_horns,
    frequency_sprite = data_sprite$frequency,
    frequency_closure = data_closure$frequency
  )
}


# Add all arguments of `sprite_generate()`.
# After the last one, add `iterations` with a default of `1`.
formals(compare_sprite_closure) <- sprite_generate |>
  formals() |>
  formals_add(
    iterations = 1,
    .after = formals_last()
  )
