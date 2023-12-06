library(data.table)
library(ggplot2)
library(rlang)
library(magrittr)
library(purrr)
library(RplotterPkg)

variables <-  c("percwhite", "percblack", "perchsd", "percollege", "percadultpoverty", "percchildbelowpovert")

build_plot <- function(id, df, variables){
  variable <- variables[id]
  plot_df <- data.frame(
    x = df[[variable]]
  )

  aplot <- RplotterPkg::create_density_plot(
    df = plot_df,
    aes_x = "x",
    rot_y_tic_label = T,
    show_minor_grids = F,
    x_title = NULL,
    y_title = "Density"
  )

  return(aplot)
}

plot_lst <- purrr::map(1:6,
  build_plot,
  df = ggplot2::midwest,
  variables = variables
)

layout <- list(
  plots = plot_lst,
  rows = c(1, 1, 2, 2, 3, 3),
  cols = c(1, 2, 1, 2, 1, 2)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  title = "Density Distributions for Selected ggplot2::midwest % Variables",
  plot_titles = c("percwhite", "percblack", "perchsd", "percollege", "percadultpoverty", "percchildbelowpovert"),
  y_tick_width = 1,
  cell_width = 14,
  cell_height = 6
)
