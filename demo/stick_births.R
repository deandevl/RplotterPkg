library(ggplot2)
library(tsbox)
library(astsa)
library(rlang)
library(RtsaPkg)
library(RplotterPkg)

# Births in US
births_dt <- RtsaPkg::tsObj_to_dt(astsa::birth)

str(births_dt)

RplotterPkg::create_stick_plot(
  df = births_dt,
  aes_x = "time",
  aes_y = "value",
  subtitle = "Time Series for US Births",
  y_title = "Number",
  rot_y_tic_label = TRUE,
  line_width = 1.2,
  line_color = "lightblue",
  x_major_breaks = seq(as.Date("1948-01-01"), as.Date("1979-01-01"), "2 year"),
  x_date_labels = "%Y"
)
