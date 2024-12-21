library(data.table)
library(ggplot2)
library(here)
library(RplotterPkg)

data_path <- file.path(here(), "demo", "data", "boston_marathon.txt")
marathon_dt <- data.table::fread(data_path) |>
  na.omit()

RplotterPkg::create_box_plot(
  df = marathon_dt[age == 20,],
  aes_y = "time",
  box_color = "blue",
  y_limits = c(150, 280),
  y_major_breaks = seq(150, 280, 10),
  show_minor_grids = FALSE,
  do_coord_flip = TRUE
)
