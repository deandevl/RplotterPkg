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
  output_dir = output_dir,
  scale_breaks = seq(70,100,5),
  scale_labels = seq(70,100,5),
  scale_limits = c(70,100),
  scale_palette = "GnBu",
  display_plot = F
)

computers_2021_lst <- RcensusPkg::plot_us_data(
  df = acs1_computers_data_2021_dt[!(State %in% c("Alaska","Hawaii","Puerto Rico")),],
  states_col = "State",
  value_col = "ComputerPresent",
  output_dir = output_dir,
  scale_breaks = seq(70,100,5),
  scale_labels = seq(70,100,5),
  scale_limits = c(70,100),
  scale_palette = "GnBu",
  display_plot = FALSE
)

plot_lst <- list(computers_2013_lst$us_states, computers_2021_lst$us_states)

layout <- list(
  plots = plot_lst,
  rows = c(1, 2),
  cols = c(1, 1)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  cell_width = 15,
  cell_height = 9,
  plot_titles = c("Year: 2013","Year:2021")
)
