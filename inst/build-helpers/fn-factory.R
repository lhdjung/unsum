# NOTE: Using this function would require:
# -- 1. Loading fn-formals.R first
# -- 2. Loading the file that contains `generate_from_mean_sd_n()`
# -- 3. No longer using `formals_adapt_generator()`
# -- 4. Possibly changing the `source()` in utils.R to load this file instead
# The difficulty is with step 2 because `generate_from_mean_sd_n()` is needed at
# runtime.

source("R/generate.R")
source("inst/build-helpers/fn-formals.R")

# Get the list of formals arguments from `generate_from_mean_sd_n()`, then
# reduce it to the subset of arguments that exported sample generator functions
# like `closure_generate()` should have.
formals_generator_final <- generate_from_mean_sd_n |>
  formals() |>
  formals_remove(
    "technique",
    "rounding_error_mean",
    "rounding_error_sd"
  )

# Remove the generator function because it is no longer needed for building. It
# will, of course, be present in the final binary because it is under R/ and
# needs to be called at runtime, but here, `rm()` just removes the manually
# sourced copy.
rm(generate_from_mean_sd_n)

# Build helper that constructs functions like `closure_generate()`. The output
# function is just a wrapper around `generate_from_mean_sd_n()` that passes
# user-supplied arguments to the formal arguments -- which, in turn, are simply
# the `formals_generator_final` list. The only real exception is `technique`:
# the outer function takes a `technique` string like "CLOSURE" and inserts it to
# specify the `technique` argument of `generate_from_mean_sd_n()`.
new_generator_mean_sd_n <- function(technique) {
  rlang::new_function(
    args = formals_generator_final,
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
