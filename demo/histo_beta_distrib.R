library(data.table)
library(here)
library(data.table)
library(ggplot2)
library(magrittr)
library(RplotterPkg)

#  Read in the 2018 ANES pilot study for Donald Trump:
data_path <- file.path(here::here(), "demo/data/therms18.rda")
load(data_path)

therms_dt <- data.table::as.data.table(Therms18) %>%
  .[, .(fttrump)] %>%
  na.omit()

# Plot the histogram of the Trump ratings:
RplotterPkg::create_histogram_plot(
  df = therms_dt,
  aes_x = "fttrump",
  title = "The Thermometer Ratings for Donald Trump",
  subtitle = "Source: ANES' 2018 Pilot Study",
  bar_fill = "blue",
  bar_alpha = 0.5,
  rot_y_tic_label = TRUE,
  x_major_breaks = seq(0,100,10),
  y_limits = c(0,500),
  y_major_breaks = seq(0,500,100)
)
