
test_that("The required dataframe/aes_x parameters are submitted and
  a .png file of the plot is created.", {

  plot_file <- "density_plot.png"

  a_plot <- RplotterPkg::create_density_plot(
    df = datasets::airquality,
    aes_x = "Ozone",
    rot_y_tic_label = TRUE,
    x_limits = c(-40,200),
    x_major_breaks = seq(-40,200,20),
    plot_obs = TRUE,
    plot_obs_jitter = TRUE,
    density_fill = "green",
    density_alpha = 0.5,
    silent_NA_warning = TRUE,
    png_file_path = plot_file
  )
  show(a_plot)

  expect_true(is.ggplot(a_plot))
  expect_true(file.exists(plot_file))
  file.remove(plot_file)
})
