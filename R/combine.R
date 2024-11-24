
#' Create CLOSURE combinations
#'
#' @param mean
#' @param sd
#' @param n
#' @param scale_min
#' @param scale_max
#' @param rounding_error_mean
#' @param rounding_error_sd
#'
#' @return
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
    tibble::as_tibble(.name_repair = function(x) paste0("n", seq_along(x))) %>%
    add_class("closure_combine")
}

