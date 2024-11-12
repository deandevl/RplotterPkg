library(ggplot2)
library(ggrepel)
library(data.table)
library(here)
library(RplotterPkg)

# Show degree of symmetry in farm counts across US states
data_path <- file.path(here(), "demo", "data", "farms.txt")
farms_dt <- data.table::fread(data_path)

RplotterPkg::symmetry_plot(
  df = farms_dt,
  var_name = "count",
  rot_y_tic_label = TRUE,
  line_linetype = "dotted"
)
