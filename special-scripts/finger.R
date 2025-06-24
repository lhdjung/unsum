
library(unsum)

closure_generate(
  mean = "3.00",
  sd = "0.80",
  n = 60,
  scale_min = 1,
  scale_max = 5
) |>
  closure_plot_bar() +
  ggplot2::labs(title = "The finger of no confidence") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
