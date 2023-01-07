library(ggplot2)
library(data.table)
library(socviz)
library(magrittr)
library(RColorBrewer)
library(RplotterPkg)

religion_happy_percent_dt <- data.table::as.data.table(socviz::gss_sm) %>%
  .[!is.na(religion) & !is.na(happy)] %>%
  .[, .(N = .N),   by = .(happy, religion)] %>%
  .[, .(happy = happy, N = N, Total = sum(N), Percent = N/sum(N)), by = religion]

religion_happy_percent_plot <- RplotterPkg::create_bar_plot(
  df = religion_happy_percent_dt,
  aes_x = "religion",
  aes_y = "Percent",
  aes_fill = "happy",
  position = "dodge",
  rot_y_tic_label = T,
  x_title = "Religion",
  y_title = "Percent"
)

religion_happy_percent_plot
