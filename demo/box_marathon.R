library(data.table)
library(ggplot2)
library(here)
library(magrittr)
library(RplotterPkg)

data_path <- file.path(here(), "demo", "data", "boston_marathon.txt")
marathon_dt <- data.table::fread(data_path) %>%
  na.omit(.)

RplotterPkg::create_box_plot(
  df = marathon_dt[age == 20,],
  aes_y = "time",
  do_coord_flip = TRUE
)