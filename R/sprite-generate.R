#' Generate SPRITE samples
#'
#' @description
#' Generate samples using the SPRITE (Sample Parameter Reconstruction via
#' Iterative Techniques) algorithm. SPRITE reconstructs possible sample
#' distributions given summary statistics and multi-item scale information.
#'
#' **Important:** `stop_after` is required when `n_items > 1` to prevent
#' overflow errors in the current implementation.
#'
#' @param n_items Numeric (length 1). Number of items/questions in your scale.
#'   Must be at least 2. This represents how many individual items were averaged
#'   to produce each participant's mean score.
#' @param stop_after Numeric (length 1). **Required** for SPRITE when `n_items > 1`.
#'   Limits the number of samples returned to prevent overflow. Recommended value:
#'   100-1000 depending on your needs.
#' @param dont_test Logical (length 1). If `TRUE`, skip GRIMMER validation tests.
#'   Automatically enabled when `n_items > 1`. Default is `FALSE`.
#' @inheritParams closure_generate
#'
#' @inheritSection closure_generate Rounding limitations
#'
#' @return `r closure_to_sprite_return(generate_return)`
#'
#' @include doc-sections.R
#' @export
#'
#' @examples
#' # Basic example with 2 items
#' \dontrun{
#' data_simple <- sprite_generate(
#'   mean = "3.0",
#'   sd = "1.0",
#'   n = 10,
#'   n_items = 2,
#'   scale_min = 1,
#'   scale_max = 5,
#'   stop_after = 100
#' )
#'
#' # Larger example - use stop_after to avoid overflow
#' data_high <- sprite_generate(
#'   mean = "3.5",
#'   sd = "1.7",
#'   n = 70,
#'   n_items = 5,
#'   scale_min = 1,
#'   scale_max = 5,
#'   stop_after = 100  # Recommended for large parameter combinations
#' )
#' }
#'
sprite_generate <- new_generator_mean_sd_n("SPRITE")
