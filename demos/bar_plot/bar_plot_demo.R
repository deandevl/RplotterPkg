# Title     : Demo of bar_plot() from RplotterPkg package
# Objective : Demonstrate bar_plot()
# Created by: Rick
# Created on: 2020-01-024:25 PM

library(ggplot2)
library(data.table)
library(rlang)
library(RplotterPkg)
library(socviz)
library(dplyr)

# bar plot showing counts of religions un-ordered
RplotterPkg::create_bar_plot(
  df = socviz::gss_sm,
  aes_x = "religion",
  title = "General Social Survey, 2016",
  x_title = "Religion",
  bar_color = "purple",
  bar_size = 1.2,
  rot_y_tic_label = TRUE,
  bar_labels = TRUE,
  bar_label_color = "red",
  bar_label_size = 5,
  do_coord_flip = TRUE,
  silent_NA_warning = TRUE
)

# bar plot showing counts of religions in descending order
RplotterPkg::create_bar_plot(
  df = socviz::gss_sm,
  aes_x = "religion",
  title = "General Social Survey, 2016",
  x_title = "Religion",
  bar_fill = "gold",
  bar_color = "purple",
  bar_size = 1.2,
  rot_y_tic_label = TRUE,
  bar_labels = TRUE,
  bar_label_color = "red",
  bar_label_size = 5,
  order_bars = "desc",
  silent_NA_warning = TRUE
)

# bar plot showing values of "weight" in ascending order
people_weights_df <- data.frame(
  name = as.factor(c("John", "Alice", "Peter", "Paul", "Tom")),
  weight = c(190, 110, 210, 147, 165)
)
RplotterPkg::create_bar_plot(
  df = people_weights_df,
  aes_x = "name",
  aes_y = "weight",
  title = "Weights of Employees",
  subtitle = "Porter Hardware Store",
  x_title = "Name",
  y_title = "Weight",
  bar_fill = "green",
  bar_color = "red",
  bar_size = 1.2,
  rot_y_tic_label = TRUE,
  center_titles = TRUE,
  bar_labels = TRUE,
  order_bars = "asc",
  do_coord_flip = TRUE
)

religion_df <- socviz::gss_sm %>% filter(!is.na(religion) & !is.na(happy)) %>% select(happy,religion)
#bar plot showing stacked (position = "stack") bars of "religion" counts
RplotterPkg::create_bar_plot(
  df = religion_df,
  title = "Distribution of religions across 'happy'",
  aes_x = "happy",
  aes_fill = "religion",
  position = "stack",
  palette_colors = c("blue", "black", "seagreen", "red", "brown"),
  center_titles = TRUE,
  rot_y_tic_label = TRUE
)

#bar plot showing side-by-side (position = "dodge") bars of "religion" counts
RplotterPkg::create_bar_plot(
  df = religion_df,
  title = "Distribution of religions across 'happy'",
  aes_x = "happy",
  aes_fill = "religion",
  position = "dodge",
  palette_colors = c("blue", "black", "seagreen", "red", "brown"),
  center_titles = TRUE,
  rot_y_tic_label = TRUE
)