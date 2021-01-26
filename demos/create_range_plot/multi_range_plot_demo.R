library(data.table)
library(palmerpenguins)
library(RplotterPkg)

data(penguins, package = "palmerpenguins")

str(penguins)
head(penguins)

penguins_dt <- data.table::setDT(penguins)
penguins_dt <- na.omit(penguins_dt)

penguins_stats_dt <- penguins_dt[, .(
  avg_body_mass = mean(body_mass_g),
  min_body_mass = min(body_mass_g),
  max_body_mass = max(body_mass_g)
), by = .(island,species)]

print(penguins_stats_dt)

RplotterPkg::multi_range_plot(
  df = penguins_stats_dt,
  factor_var = "island",
  factor_x = "species",
  columns = 3,
  col_width = 4,
  row_height = 4,
  aes_y = "avg_body_mass",
  aes_y_min = "min_body_mass",
  aes_y_max = "max_body_mass",
  title = "Average and Range of Penguins Body Mass(g) by Island/Species",
  subtitle = "Source: palmerpenguins",
  center_titles = T,
  x_title = "Species",
  y_titles = c("Body Mass(g)",NULL,NULL),
  rot_y_tic_label = T,
  pts_fill = "blue",
  pts_shape = 22,
  pts_stroke = 1.7,
  line_pts_size = 1.5,
  line_type = "solid",
  line_pts_color = "red",
  line_pts_alpha = 0.5,
  y_limits = c(2500, 7000),
  y_major_breaks = seq(2500,7000,500),
  show_major_grids = T,
  show_minor_grids = F
)
