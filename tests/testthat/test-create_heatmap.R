
test_that("The required dataframe/aes_x/aes_y parameters are submitted and
  a .png file of the plot is created.", {

  plot_file <- "heatmap_plot.png"

  a_plot <- RplotterPkg::create_heatmap(
    df = RplotterPkg::spinrates,
    aes_x = "velocity",
    aes_y = "spinrate",
    aes_fill = "swing_miss",
    aes_label = "swing_miss",
    label_fontface = "bold",
    title = "Likelihood of swinging and missing on a fastball",
    x_title = "Velocity",
    y_title = "Spinrate",
    rot_y_tic_label = TRUE,
    png_file_path = plot_file
  ) +
  ggplot2::scale_fill_gradientn(
    colors = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
    n.breaks = 8
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      ticks.colour = "black"
    )
  )
  show(a_plot)

  expect_true(is.ggplot(a_plot))
  expect_true(file.exists(plot_file))
  file.remove(plot_file)
})
