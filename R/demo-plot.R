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
#'   index, which is also shown; see [`horns()`].
#'
#' - `demo_plot_bar()` is like [`closure_plot_bar()`] except it is not faceted.
#' - `demo_plot_horns_histogram()` is like [`closure_plot_horns_histogram()`].
#' - `demo_plot_ecdf()` is like [`closure_plot_ecdf()`].
#'
#' @details In keeping with the forensic metascience tradition of tortured
#'   backronyms, DEMO stands for "displaying examples of meticulous operation".
#'
#' @param freqs Numeric. Vector of relative or absolute frequencies to
#'   visualize.
#' @param bar_color String (length 1). Color of the bars. Default is
#'   `"darkgreen"`.
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

demo_plot_bar <- new_plot_fn_bar("DEMO", "darkgreen")
