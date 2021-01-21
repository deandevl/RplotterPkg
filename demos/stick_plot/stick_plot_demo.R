# Title     : Demo the create_stick_plot() function
# Objective : Show examples using create_stick_plot()
# Created by: Rick Dean
# Created on: 2020-02-23 7:18 AM

library(ggplot2)
library(tseries)
library(RplotterPkg)

data("ice.river")

# the "prec" series of ice.river
ice_river_df <- RtsaPkg::ts_to_df(ice.river)

str(ice_river_df)

RplotterPkg::create_stick_plot(
  df = ice_river_df,
  aes_x = "DateTime",
  aes_y = "Value.prec",
  title = "Time Series for Daily Precipitation in Hveravellir (mm)",
  subtitle = "Jan 1972 to Dec 1974",
  y_title = "Precipitation",
  y_major_breaks = seq(0,80,10),
  rot_y_tic_label = TRUE
)

# AirPassergers time series
AirPassengers_df <- RtsaPkg::ts_to_df(ts_obj = AirPassengers)
str(AirPassengers_df)
RplotterPkg::create_stick_plot(
  df = AirPassengers_df,
  aes_x = "DateTime",
  aes_y = "Value",
  title = "Monthly Totals of International Airline Passengers",
  subtitle = "1949 - 1960 (classic Box & Jenkins)",
  x_major_date_breaks = "1 year",
  x_date_labels = "%Y",
  rot_y_tic_label = TRUE,
  show_minor_grids = FALSE,
  bold_y = 0.0
)

