library(here)
library(data.table)
library(ggplot2)
library(ggrepel)
library(RplotterPkg)

# Read the data: baseball homeruns from years 1900 to 2000
data_path <- file.path(here(), "demo", "data", "homeruns_2000.txt")
homeruns_dt <- data.table::fread(data_path) |>
  _[, YEARS := as.factor(YEARS)]

# Show the boxplot of homeruns for each YEARS levels
RplotterPkg::create_box_plot(
  df = homeruns_dt,
  aes_x = "YEARS",
  aes_y = "HOMERUNS",
  box_color = "blue",
  show_minor_grids = FALSE,
  do_coord_flip = TRUE,
  x_title = "Years"
)

# Note that the spreads across the years are not the same--small
#   homerun numbers are also the ones with smallest spreads.
# There appears to be a dependence between spread and median values.

# Show the scatter plot of median versus spread(difference between 75th and 25th quartile)
#   log10 values for each year.
spread_level_lst <- RplotterPkg::spread_level_plot(
  df = homeruns_dt,
  meas_var = "HOMERUNS",
  factor_var = "YEARS",
  x_title = "Log Median",
  y_title = "Log Spread"
)
# plot the median vs. spread
spread_level_lst$scatter_plot

# There appears to be an association with a fit line slope of .78.
