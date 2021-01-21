# Objective : Demo RplotterPkg::multi_bar_plot()
# Created by: Rick Dean
# Created on: 2020-05-30 6:30 AM

library(data.table)
library(ggplot2)
library(rlang)
library(grid)
library(gtable)
library(socviz)
library(RplotterPkg)
library(dplyr)

# multi_bar_plot() values of "religion" across  levels of "bigregion" in "Percent" rather than count.
percent_religion_df <- socviz::gss_sm %>%
  filter(!is.na(religion)) %>%
  group_by(bigregion, religion) %>%
  summarise(n_relig = n()) %>%  # religion is peeled off; is now grouped by bigregion
  mutate(Percent = n_relig/sum(n_relig) * 100) # sum(n_relig) is the total by bigregion
str(percent_religion_df)

RplotterPkg::multi_bar_plot(
  df = percent_religion_df,
  factor_var = "bigregion",
  factor_x = "religion",
  aes_y = "Percent",
  y_limits = c(0,70),
  y_major_breaks = seq(from = 0, to = 70, by = 10),
  bar_fill = "gold",
  bar_color = "black",
  title = "Percent of religions across US regions",
  subtitle = "Source: socviz::gss_sm",
  x_title = "Religion",
  rot_y_tic_label = TRUE
)

# multi_bar_plot() diamond values of "cut" across levels of "clarity" in count.
RplotterPkg::multi_bar_plot(
  df = ggplot2::diamonds,
  factor_var = "clarity",
  factor_x = "cut",
  columns = 3,
  title = "Count of diamond 'cut' across of levels of 'clarity'",
  rot_y_tic_label = TRUE,
  bar_color = "black",
  bar_fill = "green",
  y_limits = c(0, 5000),
  y_major_breaks = seq(from =0,to = 5000, by = 500),
  col_width = 4.5,
  row_height = 3,
  silent_NA_warning = TRUE
)

# multi_bar_plot() of some factor variables in socviz::gss_sm
RplotterPkg::multi_bar_plot(
  df = socviz::gss_sm,
  variables = c("sex", "race", "region", "polviews", "relig", "marital"),
  title = "Counts of variables from socviz::gss_sm",
  col_width = 8,
  rot_y_tic_label = TRUE,
  do_coord_flip = TRUE,
  order_bars = TRUE
)