library(ggplot2)
library(tsbox)
library(RtsaPkg)
library(RplotterPkg)

AirPassengers_dt <- RtsaPkg::tsObj_to_dt(datasets::AirPassengers)
str(AirPassengers_dt)

RplotterPkg::create_stick_plot(
  df = AirPassengers_dt,
  aes_x = "time",
  aes_y = "value",
  title = "Monthly Totals of International Airline Passengers",
  subtitle = "1949 - 1960 (classic Box & Jenkins)",
  x_title = "Time",
  y_title = "Totals",
  x_major_date_breaks = "1 year",
  x_date_labels = "%Y",
  rot_y_tic_label = TRUE,
  show_minor_grids = FALSE,
  bold_y = 0.0
)
