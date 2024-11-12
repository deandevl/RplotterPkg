library(data.table)
library(jsonlite)
library(httr)
library(usmap)
library(purrr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(here)
library(RspatialPkg)
library(RcensusPkg)
library(RplotterPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

acs1_computers_data_2013_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2013,
  vars = c("DP02_0150E", "DP02_0151PE", "DP02_0152PE"),
  region = "state:*"
)
acs1_computers_data_2013_dt <- acs1_computers_data_2013_dt %>%
  data.table::setnames(
    old = c("NAME", "DP02_0150E", "DP02_0151PE", "DP02_0152PE"),
    new = c("State", "Total", "ComputerPresent", "BroadbandPresent")) %>%
  .[, .(GEOID, State, Total, ComputerPresent, BroadbandPresent)] %>%
  .[, `:=`(Total = as.numeric(Total), ComputerPresent = as.numeric(ComputerPresent),
           BroadbandPresent = as.numeric(BroadbandPresent))] %>%
  .[order(State)]


acs1_computers_data_2021_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2021,
  vars = c("DP02_0152E", "DP02_0153PE", "DP02_0154PE"),
  region = "state:*"
)
acs1_computers_data_2021_dt <- acs1_computers_data_2021_dt %>%
  data.table::setnames(
    old = c("NAME", "DP02_0152E", "DP02_0153PE", "DP02_0154PE"),
    new = c("State", "Total", "ComputerPresent", "BroadbandPresent")) %>%
  .[, `:=`(Total = as.numeric(Total), ComputerPresent = as.numeric(ComputerPresent),
           BroadbandPresent = as.numeric(BroadbandPresent))]

computers_2013_lst <- RcensusPkg::plot_us_data(
  df = acs1_computers_data_2013_dt[!(State %in% c("Alaska","Hawaii","Puerto Rico")),],
  states_col = "State",
  value_col = "ComputerPresent",
  title = "Year: 2013",
  output_dir = output_dir,
  scale_breaks = seq(70,100,5),
  scale_labels = seq(70,100,5),
  scale_limits = c(70,100),
  display_plot = FALSE,
  legend_key_width = 1.0
)

computers_2021_lst <- RcensusPkg::plot_us_data(
  df = acs1_computers_data_2021_dt[!(State %in% c("Alaska","Hawaii","Puerto Rico")),],
  states_col = "State",
  value_col = "ComputerPresent",
  title = "Year:2021",
  output_dir = output_dir,
  scale_breaks = seq(70,100,5),
  scale_labels = seq(70,100,5),
  scale_limits = c(70,100),
  display_plot = FALSE,
  legend_key_width = 1.0
)

plot_lst <- list(computers_2013_lst$plots$lower_48, computers_2021_lst$plots$lower_48)

layout <- list(
  plots = plot_lst,
  rows = c(1, 1),
  cols = c(1, 2)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  cell_width = 20,
  cell_height = 12,
  title = "Computers Present 2013 - 2021",
  plot_titles = c("Year: 2013","Year:2021")
)
