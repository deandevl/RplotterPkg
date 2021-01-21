library(grid)
library(gtable)
library(dplyr)
library(tibble)
library(ggplot2)
library(RplotterPkg)

# create a summary data frame of mtcars
car_stats_df <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    mean_mpg = round(mean(mpg),2),
    mean_hp = round(mean(hp),2),
    mean_wt = round(mean(wt),2),
    mean_disp = round(mean(disp),2),
    n = n()
  )

# create a graphic table of the above stats
# get the grob without having to display it, which is the default
car_stats_table_grob <- RplotterPkg::create_table_graphic(
  df = car_stats_df,
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
mtcars_df <- mtcars %>%
  mutate(cyl = as.factor(cyl)) %>%
  rename(Cylinder = cyl)

# create a scatter plot object of hp vs mpg
hp_mpg_plot <- RplotterPkg::create_scatter_plot(
  df = mtcars_df,
  aes_x = "hp",
  aes_y = "mpg",
  aes_fill = "Cylinder",
  pts_size = 2.0,
  palette_colors = c("green","blue","red"),
  x_title = "Gross Horsepower",
  y_title = "Miles Per Gallon",
  x_limits = c(50, 350),
  x_major_breaks = seq(50,350,50)
)

# create a scatter plot object of wt vs mpg
wt_mpg_plot <- RplotterPkg::create_scatter_plot(
  df = mtcars_df,
  aes_x = "wt",
  aes_y = "mpg",
  aes_fill = "Cylinder",
  pts_size = 2.0,
  palette_colors = c("green","blue","red"),
  x_title =  "Weight (1000 lbs)",
  x_limits = c(1, 6),
  x_major_breaks = seq(1,6,1),
  y_labels = NULL,
  do_y_title = FALSE
)

# create a scatter plot object of disp vs mpg
disp_mpg_plot <- RplotterPkg::create_scatter_plot(
  df = mtcars_df,
  aes_x = "disp",
  aes_y = "mpg",
  aes_fill = "Cylinder",
  pts_size = 2.0,
  palette_colors = c("green","blue","red"),
  x_title = "Displacement (cubic inches)",
  x_limits = c(50, 500),
  x_major_breaks = seq(50,500,50),
  y_labels = NULL,
  do_y_title = FALSE
)

# set up a layout for placement of the above plot objects/grob in rows/columns
layout <- list(
   plots = list(hp_mpg_plot, wt_mpg_plot, disp_mpg_plot, car_stats_table_grob),
   rows = c(1, 1, 1, 2),
   cols = c(1, 2, 3, list(1:3))
)

# with the layout, produce a multi-paneled graphic
multi_plot <- RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(3,3,3),
  row_heights = c(3,2),
  title = "MPG Measures",
  subtitle = "Motor Trend Car Road Tests (32 observations)",
  do_legend = FALSE,
  display_plot = TRUE
)


