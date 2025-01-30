# create_density_ridge_plot()

    Code
      variables_v <- c("HS_Diploma", "College_Edu", "Prof_Deg", "White", "Black",
        "Asian")
      a_plot <- RplotterPkg::create_density_ridge_plot(df = RplotterPkg::midwest, bw = "sj",
      variables = variables_v, title = "Percent Distribution Among Midwest Counties",
      x_limits = c(0, 100), x_major_breaks = seq(0, 100, 10), density_fill = "blue",
      density_alpha = 0.5, plot_heights = 2.5, plot_widths = 18)

