library(ggplot2)
library(data.table)
library(socviz)
library(RplotterPkg)

religion_happy_percent_dt <- data.table::as.data.table(socviz::gss_sm) |>
  _[!is.na(religion) & !is.na(happy)] |>
  _[, .(N = .N),   by = .(happy, religion)] |>
  _[, .(happy = happy, N = N, Total = sum(N), Percent = N/sum(N)), by = religion]

RplotterPkg::create_bar_plot(
  df = religion_happy_percent_dt,
  aes_x = "religion",
  aes_y = "Percent",
  aes_fill = "happy",
  position = "dodge",
  rot_y_tic_label = TRUE,
  x_title = "Religion",
  y_title = "Percent"
)
