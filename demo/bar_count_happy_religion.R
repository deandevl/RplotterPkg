library(ggplot2)
library(data.table)
library(rlang)
library(socviz)
library(magrittr)
library(RColorBrewer)
library(RplotterPkg)

religion_happy_dt <- data.table::as.data.table(socviz::gss_sm) %>%
  .[!is.na(religion) & !is.na(happy), .(happy, religion)]

happy_religion_plot <- RplotterPkg::create_bar_plot(
  df = religion_happy_dt,
  aes_x = "happy",
  aes_fill = "religion",
  position = "dodge",
  rot_y_tic_label = T,
  x_title = "Happiness",
  y_title = "Count"
) +
ggplot2::scale_fill_discrete(
  type = RColorBrewer::brewer.pal(n = 9, name = "Set1")
)

happy_religion_plot
