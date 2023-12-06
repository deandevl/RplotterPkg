library(grid)
library(gtable)
library(rlang)
library(data.table)
library(magrittr)
library(tibble)
library(ggplot2)
library(RplotterPkg)

# create a summary data frame of mtcars
car_stats_dt <- data.table::as.data.table(mtcars) %>%
  .[, .(
    N = .N,
    mean_mpg = round(mean(mpg),2),
    mean_hp = round(mean(hp),2),
    mean_wt = round(mean(wt),2),
    mean_disp = round(mean(disp),2)
    ), by = cyl]

# create a graphic table of the above stats
# get the grob without having to display it, which is the default
car_stats_table_grob <- RplotterPkg::create_table_graphic(
  df = car_stats_dt,
  table_width = 7,
  show_row_names = TRUE,
  cell_just = "right",
  cell_hor_pos = 0.95,
  title = "Average Measures by Cylinders",
  display_plot = FALSE
)

# to display "car_stats_table_grob" you must use grid::grid.draw()
grid::grid.draw(car_stats_table_grob)

# define a data frame for mtcars for creating a scatter plot
mtcars_dt <- data.table::as.data.table(mtcars) %>%
  .[, cyl := as.factor(cyl)] %>%
  data.table::setnames(., old = "cyl", new = "Cylinder")

# create a scatter plot object of hp vs mpg
hp_mpg_plot <- RplotterPkg::create_scatter_plot(
  df = mtcars_dt,
  aes_x = "hp",
  aes_y = "mpg",
  aes_fill = "Cylinder",
  pts_size = 2.0,
  x_title = "Gross Horsepower",
  y_title = "Miles Per Gallon",
  x_limits = c(50, 350),
  x_major_breaks = seq(50,350,50),
  show_legend = F
)

# create a scatter plot object of wt vs mpg
wt_mpg_plot <- RplotterPkg::create_scatter_plot(
  df = mtcars_dt,
  aes_x = "wt",
  aes_y = "mpg",
  aes_fill = "Cylinder",
  pts_size = 2.0,
  x_title =  "Weight (1000 lbs)",
  x_limits = c(1, 6),
  y_title = NULL,
  x_major_breaks = seq(1,6,1),
  y_labels = NULL,
  show_legend = F
)

# create a scatter plot object of disp vs mpg
disp_mpg_plot <- RplotterPkg::create_scatter_plot(
  df = mtcars_dt,
  aes_x = "disp",
  aes_y = "mpg",
  aes_fill = "Cylinder",
  pts_size = 2.0,
  x_title = "Displacement (cubic inches)",
  x_limits = c(50, 500),
  x_major_breaks = seq(50,500,50),
  y_title = NULL,
  y_labels = NULL,
  show_legend = T,
  legend_key_width = 0.75,
  legend_key_height = 0.85
)

# set up a layout for placement of the above plot objects/grob in rows/columns
layout <- list(
   plots = list(hp_mpg_plot, wt_mpg_plot, disp_mpg_plot, car_stats_table_grob),
   rows = c(1, 1, 1, 2),
   cols = c(1, 2, 3, 2)
)

# with the layout, produce a multi-paneled graphic
RplotterPkg::multi_panel_grid(
  do_grid = TRUE,
  layout = layout,
  cell_width = 12,
  cell_height = 9
)
