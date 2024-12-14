library(data.table)
library(palmerpenguins)
library(magrittr)
library(RplotterPkg)

data(penguins, package = "palmerpenguins")

penguins_dt <- data.table::as.data.table(penguins) %>%
  na.omit(.)

penguins_stats_dt <- penguins_dt[, list(
  avg_body_mass = mean(body_mass_g),
  min_body_mass = min(body_mass_g),
  max_body_mass = max(body_mass_g)
), by = list(island,species)]

str(penguins_stats_dt)

islands <- levels(penguins_stats_dt$island)

build_plot <- function(id, dt, islands){
  plot_dt <- dt[island == islands[[id]], ]

  aplot <- RplotterPkg::create_range_plot(
    df = plot_dt,
    aes_x = "species",
    aes_y = "avg_body_mass",
    aes_y_min = "min_body_mass",
    aes_y_max = "max_body_mass",
    x_title = "Species",
    y_title = "Body Mass(g)",
    rot_y_tic_label = TRUE,
    pts_fill = "blue",
    pts_shape = 22,
    pts_stroke = 1.7,
    line_width = 1.5,
    line_type = "solid",
    line_pts_color = "red",
    line_pts_alpha = 0.5,
    y_limits = c(2500, 7000),
    y_major_breaks = seq(2500,7000,500),
    show_major_grids = TRUE,
    show_minor_grids = FALSE
  )
  return(aplot)
}

plot_lst <- purrr::map(
  1:3,
  build_plot,
  dt = penguins_stats_dt,
  islands = islands
)

layout <- list(
  plots = plot_lst,
  rows = c(1, 1, 1),
  cols = c(1, 2, 3)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  title = "Average and Range of Penguins Body Mass(g) by Island/Species",
  title_fontsz = 24,
  plot_titles = c("Biscoe","Dream","Torgersen"),
  y_tick_width = 1,
  cell_width = 10,
  cell_height = 12
)


