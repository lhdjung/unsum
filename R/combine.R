
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
#' @return Tibble (data frame). Each row contains one combination. The number of
#'   columns is equal to `n`, and the columns are named `"n1"`, `"n2"`, etc.
#'
#' @include utils.R extendr-wrappers.R
#'
#' @export
#'
#' @examples
#' closure_combine(
#'   mean = 5.0,
#'   sd = 2.78,
#'   n = 30,
#'   scale_min = 1,
#'   scale_max = 8,
#'   rounding_error_mean = 0.01,
#'   rounding_error_sd = 0.01
#' )


# mean <- 5.0
# sd <- 2.78
# n <- 30
# scale_min <- 1
# scale_max <- 8
# rounding_error_mean <- 0.01
# rounding_error_sd <- 0.01


closure_combine <- function(mean,
                            sd,
                            n,
                            scale_min,
                            scale_max,
                            rounding_error_mean,
                            rounding_error_sd) {

  out <- create_combinations(
    mean = mean,
    sd = sd,
    n = n,
    scale_min = scale_min,
    scale_max = scale_max,
    rounding_error_mean = rounding_error_mean,
    rounding_error_sd = rounding_error_sd
  )

  if (length(out) == 0) {
    cli::cli_abort(c(
      "No combinations found with these inputs.",
      "x" = "Data internally inconsistent.",
      "x" = "These statistics can't describe the same distribution."
    ))
    # return(add_class(tibble::tibble(), "closure_combine"))
  }

  out %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    t() %>%
    tibble::as_tibble(.name_repair = function(x) paste0("n", seq_along(x))) %>%
    add_class("closure_combine")
}

