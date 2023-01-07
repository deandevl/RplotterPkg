library(ggplot2)
library(data.table)
library(rlang)
library(socviz)
library(ggrepel)
library(RplotterPkg)

religions_count_plot <- RplotterPkg::create_bar_plot(
  df = socviz::gss_sm,
  aes_x = "religion",
  do_coord_flip = T,
  bar_fill = "green",
  bar_size = 1.5,
  bar_width = 0.5,
  order_bars = "desc",
  bar_labels = T,
  bar_label_fontface = "bold",
  title = "General Social Survey, 2016",
  x_title = "Religion",
  y_title = "Count",
  y_limit = c(0, 1500),
  y_major_breaks = seq(from = 0, to = 1500, by = 250)
)

religions_count_plot
