library(ggplot2)
library(RplotterPkg)

RplotterPkg::create_scatter_plot(
  df = ggplot2::economics,
  aes_x = "date",
  aes_y = "unemploy",
  pts_shape = 21,
  pts_fill = "black",
  line_color = "violet",
  connect = T,
  title = "US Monthly Unemployment",
  subtitle = "July, 1967 to April, 2015 (in thousands)",
  x_title = "Date",
  y_title = "Unemployment",
  rot_y_tic_label = T,
  x_date_labels = "%Y-%b",
  x_major_date_breaks = "5 year",
  y_limits = c(0, 16000),
  y_major_breaks = seq(0, 16000, 2000),
  show_minor_grids = F,
  bold_y = 8000,
  bold_y_color = "red",
  bold_y_linetype = "dashed"
)
