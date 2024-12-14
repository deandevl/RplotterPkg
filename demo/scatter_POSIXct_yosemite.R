library(ggplot2)
library(data.table)
library(here)
library(rlang)
library(RplotterPkg)

# Read in the Yosemite_temp.csv file

data_file_path <- file.path(here::here(), "demo", "data", "yosemite_temp.csv")

yosemite_dt <- data.table::fread(file = data_file_path)

str(yosemite_dt)

# Convert time to POSIXct
yosemite_dt[, time_alt := as.POSIXct(time, format = "%Y-%m-%d %H:%M")]

# Plot the datetime data of Yosemite temperatures recorded every 5 minutes.
RplotterPkg::create_scatter_plot(
  df = yosemite_dt,
  aes_x = "time_alt",
  aes_y = "y",
  rot_y_tic_label = TRUE,
  title = "Yosemite Tempertures",
  subtitle = "Every 5 minutes on 2017-05-01 for 10 hours",
  x_title = "Time Hr:Min",
  y_title = "Temperature",
  x_date_labels = "%H:%M",
  x_major_date_breaks = "30 min",
  y_limits = c(0,30),
  y_major_breaks = seq(from = 0, to = 30, by = 5),
  connect = TRUE
)
