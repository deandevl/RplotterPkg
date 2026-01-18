# create_range_plot()

    Code
      a_plot <- RplotterPkg::create_range_plot(df = RplotterPkg::penguins_stats,
      orientation = "vertical", factor = "species", min_meas = "min_body_mass",
      max_meas = "max_body_mass", mid_meas = "avg_body_mass", title = "Average and Range of Penguins Body Mass(g) by Species",
      subtitle = "Source: palmerpenguins", center_titles = TRUE, factor_title = "Species",
      meas_title = "Body Mass(g)", pts_fill = c("white", "red"), pt_mid_fill = "yellow",
      pts_size = 3, pts_shape = 21, pts_stroke = 1, line_width = 1.5, line_type = "solid",
      line_pts_alpha = 0.5, meas_limits = c(2500, 7000), meas_major_breaks = seq(2500,
        7000, 500), rot_y_tic_label = TRUE, show_major_grids = TRUE,
      show_minor_grids = FALSE)

