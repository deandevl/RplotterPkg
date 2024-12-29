
test_that("The required dataframe/aes_x parameters are submitted and
  a .png file of the plot is created.", {

  plot_file <- "bar_plot.png"

  a_plot <- RplotterPkg::create_bar_plot(
    df = RplotterPkg::religion,
    aes_x = "happy",
    aes_fill = "religion",
    position = "dodge",
    rot_y_tic_label = TRUE,
    bar_color = "black",
    bar_lwd = 2,
    bar_width = 0.8,
    x_title = "Happiness",
    y_title = "Count",
    axis_text_size = 16,
    png_file_path = plot_file
  ) +
  ggplot2::scale_fill_discrete(
    type = RColorBrewer::brewer.pal(n = 9, name = "Set1")
  )
  show(a_plot)

  expect_true(is.ggplot(a_plot))
  expect_true(file.exists(plot_file))
  file.remove(plot_file)
})
