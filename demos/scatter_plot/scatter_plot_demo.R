# Title     : scatter_plot demo
# Objective : demos the scatter_plot() function
# Created by: Rick
# Created on: 2020-01-019:29 AM

library(ggplot2)
library(RplotterPkg)

# ------------------------------------------First demo on ggplot::economics data set---------------------------
RplotterPkg::create_scatter_plot(
  df = ggplot2::economics,
  aes_x = "date",
  aes_y = "unemploy",
  x_title = "Date",
  y_title = "Unemployment",
  rot_y_tic_label = TRUE,
  title = "US Monthly Unemployment",
  subtitle = "1967 to 2014 (in thousands)",
  pts_color = "blue",
  line_color = "violet",
  connect = TRUE,
  x_date_labels = "%Y",
  x_major_breaks = seq.Date(from = as.Date("1964-01-01"), to = as.Date("2020-01-01"), by = "4 year"),
  show_minor_grids = FALSE
) + geom_hline(aes(yintercept = 8000), color = "red", linetype = "dashed", lwd = 1)