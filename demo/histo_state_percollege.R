library(ggplot2)
library(data.table)
library(rlang)
library(magrittr)
library(RColorBrewer)
library(RplotterPkg)

oh_wis_df <- data.table::as.data.table(ggplot2::midwest) %>%
  .[state %in% c("OH", "WI"), .(state, percollege)]

create_histogram_plot(
  df = oh_wis_df,
  aes_x = "percollege",
  aes_fill = "state",
  position = "dodge",
  binwidth = 2.0,
  x_major_breaks = seq(0,50,2),
  y_limits = c(0, 30),
  y_major_breaks = seq(from = 0, to = 30, by =5),
  show_minor_grids = F,
  x_title = "Percollege",
  y_title = "Count",
  bar_color = "white",
  bar_size = 1.5,
  title = "Distribution of Counties Percent in College",
  subtitle = "Counties from Ohio and Wisconsin"
) +
ggplot2::scale_fill_discrete(
  type = RColorBrewer::brewer.pal(n = 8, name = "Dark2")
)
