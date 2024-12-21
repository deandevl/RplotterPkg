library(data.table)
library(palmerpenguins)
library(RplotterPkg)

data(penguins, package = "palmerpenguins")

penguins_dt <- data.table::as.data.table(penguins) |>
  na.omit()

penguins_stats_dt <- penguins_dt[, .(
  avg_body_mass = mean(body_mass_g),
  min_body_mass = min(body_mass_g),
  max_body_mass = max(body_mass_g)
), by = species]

str(penguins_stats_dt)

RplotterPkg::create_range_plot(
  df = penguins_stats_dt,
  aes_x = "species",
  aes_y = "avg_body_mass",
  aes_y_min = "min_body_mass",
  aes_y_max = "max_body_mass",
  title = "Average and Range of Penguins Body Mass(g) by Species",
  subtitle = "Source: palmerpenguins",
  center_titles = TRUE,
  x_title = "Species",
  y_title = "Body Mass(g)",
  rot_y_tic_label = TRUE,
  pts_fill = "blue",
  pts_shape = 22,
  pts_stroke = 1.7,
  line_width = 1.5,
  fatten_pts = 6,
  line_type = "solid",
  line_pts_color = "red",
  line_pts_alpha = 0.5,
  y_limits = c(2500, 7000),
  y_major_breaks = seq(2500,7000,500),
  show_major_grids = TRUE,
  show_minor_grids = FALSE,
  do_coord_flip = TRUE
)
