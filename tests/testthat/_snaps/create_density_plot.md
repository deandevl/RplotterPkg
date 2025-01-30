# create_density_plot() aes_x x_major_breaks plot_obs

    Code
      a_plot <- RplotterPkg::create_density_plot(df = datasets::airquality, aes_x = "Ozone",
      rot_y_tic_label = TRUE, x_limits = c(-40, 200), x_major_breaks = seq(-40, 200,
        20), title = "Ozone Air Quality", x_title = "Ozone", y_title = "Density",
      plot_obs = TRUE, density_fill = "green", density_alpha = 0.5,
      silent_NA_warning = TRUE)

# create_density_plot() aes_x cum_prob=.95

    Code
      a_plot <- RplotterPkg::create_density_plot(df = datasets::airquality, aes_x = "Ozone",
      cum_prob = 0.95, rot_y_tic_label = TRUE, x_limits = c(-40, 200),
      x_major_breaks = seq(-40, 200, 20), rot_x_tic_angle = 40, density_fill = "green",
      density_alpha = 0.5, silent_NA_warning = TRUE)

# create_density_plot() aes_x cum_prob=1.00

    Code
      a_plot <- RplotterPkg::create_density_plot(df = datasets::airquality, aes_x = "Ozone",
      cum_prob = 1, x_limits = c(-40, 200), x_major_breaks = seq(-40, 200, 20),
      hide_x_tics = TRUE, hide_y_tics = TRUE, density_fill = "green", density_alpha = 0.5,
      silent_NA_warning = TRUE)

# create_density_plot() aes_x cum_prob=c(0.25,0.75)

    Code
      a_plot <- RplotterPkg::create_density_plot(df = datasets::airquality, aes_x = "Ozone",
      cum_prob = c(0.25, 0.75), x_limits = c(-40, 200), x_major_breaks = seq(-40, 200,
        20), hide_x_tics = TRUE, hide_y_tics = TRUE, density_fill = "green",
      density_alpha = 0.5, silent_NA_warning = TRUE)

