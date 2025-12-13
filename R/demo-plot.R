# TODO: Write these demo functions:
# demo_plot_horns_histogram()
# demo_plot_ecdf()

# Displaying examples of meticulous operation (DEMO) is a way to show the ideas
# behind unsum's functions without reconstructing any samples. Rather, DEMO
# functions are based on user-provided vectors of frequencies that are
# visualized in the same ways as CLOSURE results.

#' Visualize example distributions and their \eqn{h} values
#'
#' @description The `demo_plot_*()` functions are variants of `closure_plot_*()`
#'   that directly visualize a given frequency distribution. Their purpose is to
#'   illustrate general points about CLOSURE-type techniques and the horns
#'   index.
#'
#'   - `demo_plot_bar()` is like [`closure_plot_bar()`] except it is never
#'   faceted.
#'   - `demo_plot_horns_histogram()` is like [`closure_plot_horns_histogram()`].
#'   - `demo_plot_ecdf()` is like [`closure_plot_ecdf()`].
#'
#'   The top line shows the horns index of the given distribution (\eqn{h}) and
#'   the horns index of a hypothetical uniform distribution with the same number
#'   of scale points (\eqn{h_u}). See [`horns()`] and `horns_uniform()` for the
#'   corresponding functions.

#' @details In keeping with the forensic metascience tradition of tortured
#'   backronyms, DEMO stands for "displaying examples of meticulous operation".
#'
#' @param freqs Numeric. Vector of relative or absolute frequencies to
#'   visualize.
#' @inheritParams closure_plot_bar
#'
#' @returns A ggplot object.
#'
#' @rdname demo-plot
#'
#' @export
#'
#' @include utils.R fn-factory.R
#'
#' @examples
#' # Zero variance: h = 0
#' demo_plot_bar(freqs = c(0, 40, 0, 0, 0))
#'
#' # Perfect "horns of no confidence": h = 1
#' demo_plot_bar(freqs = c(20, 0, 0, 0, 20))
#'
#' # Grouped around h = ~0.44, the uniform horns index
#' # for 7-point scales
#' demo_plot_bar(freqs = 21:27)

demo_plot_bar <- new_plot_fn_bar("DEMO", "#5D3FD3")

# # Also of note: h == h_u here even though the distribution is not uniform
# demo_plot_bar(c(20, 40, 20, 20, 30))
