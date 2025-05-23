% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine.R
\name{closure_combine}
\alias{closure_combine}
\title{Create CLOSURE combinations}
\usage{
closure_combine(
  mean,
  sd,
  n,
  scale_min,
  scale_max,
  rounding = "up_or_down",
  threshold = 5,
  warn_if_empty = TRUE,
  rounding_error_mean = NULL,
  rounding_error_sd = NULL
)
}
\arguments{
\item{mean}{String (length 1). Reported mean.}

\item{sd}{String (length 1). Reported sample standard deviation.}

\item{n}{Numeric (length 1). Reported sample size.}

\item{scale_min, scale_max}{Numeric (length 1 each). Minimal and maximal
possible values of the measurement scale (not the \emph{empirical} min and
max!). For example, with a 1-7 Likert scale, use \code{scale_min = 1} and
\code{scale_max = 7}.}

\item{rounding}{String (length 1). Rounding method assumed to have created
\code{mean} and \code{sd}. See \href{https://lhdjung.github.io/roundwork/articles/rounding-options.html}{\emph{Rounding options}},
but also the \emph{Rounding limitations} section below. Default is
\code{"up_or_down"} which, e.g., unrounds \code{0.12} to \code{0.115} as a lower bound and
\code{0.125} as an upper bound.}

\item{threshold}{Numeric (length 1). Number from which to round up or down,
if \code{rounding} is any of \code{"up_or_down"}, \code{"up"}, and \code{"down"}. Default is
\code{5}.}

\item{warn_if_empty}{Logical (length 1). Should a warning be shown if no
combinations are found? Default is \code{TRUE}.}
}
\value{
Named list of four tibbles (data frames):
\itemize{
\item \strong{\code{inputs}}: Arguments to this function.
\item \strong{\code{metrics}}:
\itemize{
\item \code{combos_initial}: integer. The basis for computing CLOSURE results,
based on scale range only.
\item \code{combos_all}: integer. Number of all combinations. Equal to the number
of rows in \code{results}.
\item \code{values_all}: integer. Number of all individual values found. Equal to
\code{n * combos_all}.
\item \code{horns}: double. Measure of dispersion for bounded scales; see
\code{\link[=horns]{horns()}}.
\item \code{horns_uniform}: double. Value \code{horns} would have if the reconstructed
sample was uniformly distributed.
}
\item \strong{\code{frequency}}:
\itemize{
\item \code{value}: integer. Scale values derived from \code{scale_min} and
\code{scale_max}.
\item \code{f_absolute}: integer. Count of individual scale values found in the
\code{results} combinations.
\item \code{f_relative}: double. Values' share of total values found.
}
\item \strong{\code{results}}:
\itemize{
\item \code{id}: integer. Runs from \code{1} to \code{combos_all}.
\item \code{combination}: list of integer vectors. Each of these vectors has
length \code{n}. It is a combination (or distribution) of individual scale
values found by CLOSURE.
}
}
}
\description{
Call \code{closure_combine()} to run the CLOSURE algorithm on a given
set of summary statistics.

This can take seconds, minutes, or longer, depending on the input. Wide
variance and large samples often lead to many combinations, i.e., long
runtimes. These effects interact dynamically. For example, with large \code{n},
even very small increases in \code{sd} can greatly increase runtime and number
of values found.

If the inputs are mutually inconsistent, there is a warning and an empty
data frame.
}
\section{Rounding limitations}{
 The \code{rounding} and \code{threshold} arguments are
not fully implemented. For example, CLOSURE currently treats all rounding
bounds as inclusive, even if the \code{rounding} specification would imply
otherwise.

Many specifications of the two arguments will not make any difference, and
those that do will most likely lead to empty results.
}

\examples{
# High spread often leads to many combinations --
# here, 735.
data_high <- closure_combine(
  mean = "3.5",
  sd = "2",
  n = 52,
  scale_min = 1,
  scale_max = 5
)

data_high

# Get a clear picture of the distribution
# by following up with `closure_plot_bar()`:
closure_plot_bar(data_high)

# Low spread, only 3 combinations, and not all
# scale values are possible.
data_low <- closure_combine(
  mean = "3.5",
  sd = "0.5",
  n = 52,
  scale_min = 1,
  scale_max = 5
)

data_low

# This can also be shown by `closure_plot_bar()`:
closure_plot_bar(data_low)
}
