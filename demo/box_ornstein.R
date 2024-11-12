library(data.table)
library(ggplot2)
library(here)
library(magrittr)
library(RplotterPkg)

data_path <- file.path(here(),"demo", "data", "Ornstein.txt")
ornstein_dt <- data.table::fread(data_path) %>%
  .[, .(nation, interlocks)] %>%
  .[, nation := factor(nation)]

RplotterPkg::create_box_plot(
  df = ornstein_dt,
  aes_x = "nation",
  aes_y = "interlocks",
  order_by_median = "desc",
  x_title = "Nation of Control",
  y_title = "Number of Interlocks"
)
