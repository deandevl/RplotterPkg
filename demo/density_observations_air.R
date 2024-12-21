library(ggplot2)
library(rlang)
library(data.table)
library(RplotterPkg)

RplotterPkg::create_density_plot(
  df = datasets::airquality,
  aes_x = "Ozone",
  rot_y_tic_label = TRUE,
  x_limits = c(-40,200),
  x_major_breaks = seq(-40,200,20),
  plot_obs = TRUE,
  plot_obs_jitter = TRUE,
  density_fill = "green",
  density_alpha = 0.5,
  silent_NA_warning = TRUE
)
