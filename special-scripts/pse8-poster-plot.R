library(unsum)
library(ggplot2)
library(scales)

# From the `closure_generate()` docs:
data_high <- closure_generate(
  mean = "3.5",
  sd = "1.7",
  n = 70,
  scale_min = 1,
  scale_max = 5
)


plot <- data_high |>
  closure_plot_bar(text_offset = 0.04) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = ggplot2::expansion(c(0.00, 0.05))
  ) +
  # labs(y = NULL) +
  theme(
    panel.grid.minor.y = element_blank(),
    axis.ticks.length.x = unit(0, "cm")
  )

plot


# Save in HIGH resolution for poster! Max is 5393
ggsave(
  "plot-for-poster.svg",
  plot = plot,
  device = "svg",
  height = 11.42,
  width = 13.9,
  units = "cm",
  dpi = 5393
)
