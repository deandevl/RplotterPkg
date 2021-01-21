# Objective : Demo RplotterPkg::multi_density_plot()
# Created by: Rick Dean
# Created on: 2020-05-31 6:14 AM

library(data.table)
library(rlang)
library(grid)
library(gtable)
library(ggplot2)
library(socviz)
library(caret)
library(dplyr)
library(RplotterPkg)

# Distribution densities of some variables of the ggplot2::midwest data set.
RplotterPkg::multi_density_plot(
  df = ggplot2::midwest,
  variables = c("percwhite", "percblack", "perchsd", "percollege", "percadultpoverty", "percchildbelowpovert"),
  title = "Density Distributions for Selected ggplot2::midwest Variables"
)

# From the socviz::opiates dataset show the density of opiates deaths across 3 years and US regions.
opiates_df <- socviz::opiates %>%
  filter(year > 2011) %>%
  mutate(year = factor(year)) %>%
  mutate(region = factor(region)) %>%
  rename(
    Year = year,
    Deaths = deaths,
    Region = region
  )
str(opiates_df)

RplotterPkg::multi_density_plot(
  df = opiates_df,
  position = "identity",
  factor_var = "Region",
  factor_x = "Deaths",
  aes_color = "Year",
  palette_colors = c("red","green","blue"),
  density_alpha = 0.6,
  title = "Opiate Deaths Across 3 Years and Regions",
  subtitle = "dataset: socviz::opiates",
  x_limits = c(0,2500),
  x_major_breaks = seq(from = 0, to = 2500, by = 500),
  y_limits = c(0.0, 0.004),
  y_major_breaks = seq(from = 0.0, to = 0.004, by = 0.001),
  rot_y_tic_label = TRUE,
  col_width = 4,
  row_height = 4
)

# From the caret::BloodBrain show densities of "peoe_vsa"
data(BloodBrain)
str(bbbDescr)

bloodbrain_peoe_df <- as.data.frame(bbbDescr %>%
  select(starts_with("peoe_")) %>%
  scale(center = TRUE, scale = TRUE))
str(bloodbrain_peoe_df)

RplotterPkg::multi_density_plot(
  df = bloodbrain_peoe_df,
  variables = colnames(bloodbrain_peoe_df),
  title = "Density Distribution for peoe_vsa",
  subtitle = "data source: caret:: BloodBrain",
  columns = 7,
  col_width = 2.0,
  row_height = 2.0,
  x_limits = c(-4,4),
  x_major_breaks = seq(-4,4,1),
  rot_y_tic_label = TRUE
)


