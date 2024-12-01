
#' Create CLOSURE combinations
#'
#' @description Call `closure_combine()` to run the CLOSURE algorithm on a given
#'   set of summary statistics.
#'
#'   This can take a few seconds or even longer, depending on the input. Wide
#'   variance often leads to many combinations, i.e., long runtimes.
#'
#' @param mean
#' @param sd
#' @param n
#' @param scale_min
#' @param scale_max
#' @param rounding_error_mean
#' @param rounding_error_sd
#'
#' @return Named list with these elements:
#'   - `results`: Tibble (data frame). Each row contains one combination. The
#'   number of columns is equal to `n`, and the columns are named `"n1"`,
#'   `"n2"`, etc.
#'   - `mean`, `sd`, `n`, `scale_min`, `scale_max`: Inputs recorded here for
#'   downstream functions.
#'
#' @include utils.R extendr-wrappers.R
#'
#' @export
#'
#' @examples
#' # High spread:
#' closure_combine(
#'   mean = 5.0,
#'   sd = 2.78,
#'   n = 30,
#'   scale_min = 1,
#'   scale_max = 8,
#'   rounding_error_mean = 0.01,
#'   rounding_error_sd = 0.01
#' )
#'
#' # Low spread, and not all scale values are possible
#' # (this becomes clearer when following up with `closure_plot_bar()`):
#' closure_combine(
#'   mean = 2.9,
#'   sd = 0.3,
#'   n = 30,
#'   scale_min = 1,
#'   scale_max = 5,
#'   rounding_error_mean = 0.01,
#'   rounding_error_sd = 0.01
#' )


# # For interactive testing:
# mean <- 5.0
# sd <- 2.78
# n <- 30
# scale_min <- 1
# scale_max <- 8
# rounding_error_mean <- 0.01
# rounding_error_sd <- 0.01


# Note: most helper functions called here can be found in the R/utils.R file.
# The only exception, `create_combinations()`, is in R/extendr-wrappers.R, but
# all it does is to call into Rust code in scr/rust/src/lib.rs which, in turn,
# accesses closure-core, a Rust crate (roughly analogous to an R package) that
# contains the actual implementation of CLOSURE:
# https://github.com/lhdjung/closure-core/blob/master/src/lib.rs

closure_combine <- function(mean,
                            sd,
                            n,
                            scale_min,
                            scale_max,
                            rounding_error_mean,
                            rounding_error_sd) {

  check_scale(scale_min, scale_max, mean)

  mean %>%
    create_combinations(
      sd = sd,
      n = n,
      scale_min = scale_min,
      scale_max = scale_max,
      rounding_error_mean = rounding_error_mean,
      rounding_error_sd = rounding_error_sd
    ) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    t() %>%
    tibble::as_tibble(
      .name_repair = function(results) {
        if (length(results) == 0) {
          cli::cli_warn(c(
            "No results found with these inputs.",
            "x" = "Data internally inconsistent.",
            "x" = "These statistics can't describe the same distribution."
          ))
          character(0)
        } else {
          paste0("n", seq_along(results))
        }
      }
    ) %>%
    add_class("closure_combine") %>%
    list_with_metadata(
      list(
        mean = mean,
        sd = sd,
        n = n,
        scale_min = scale_min,
        scale_max = scale_max
      )
    )

}

