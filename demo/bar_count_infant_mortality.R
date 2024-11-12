library(data.table)
library(ggplot2)
library(magrittr)
library(here)
library(RplotterPkg)

data_path <- file.path(here(), "demo", "data", "UnitedNations.txt")
un_dt <- data.table::fread(data_path) %>%
  .[, .(infantMortality)] %>%
  na.omit(.)
infant_mortality_intervals <- seq(0,170,by=10)

RplotterPkg::create_bar_plot(
  df = un_dt,
  aes_x = "infantMortality",
  x_major_breaks = infant_mortality_intervals,
  bar_fill = "blue",
  bar_alpha = 0.6,
  x_title = "Infant Mortality Rate(per 1000)",
  y_title = "Frequency",
  rot_y_tic_label = T
)
