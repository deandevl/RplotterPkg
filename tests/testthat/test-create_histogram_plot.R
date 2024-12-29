
test_that("The required dataframe/aes_x parameters are submitted and
  a .png file of the plot is created.", {

  plot_file <- "histo_plot.png"

  a_plot <- RplotterPkg::create_histogram_plot(
    df = RplotterPkg::midwest,
    aes_x = "Area",
    binwidth = 0.01,
    x_limits = c(0.0, 0.11),
    x_major_breaks = seq(0.0, 0.11, 0.01),
    title = "Distribution of area",
    subtitle = "437 counties from midwest dataset",
    x_title = "Area",
    y_title = "Count",
    bar_color = "white",
    bar_lwd = 2.0,
    bar_fill = "brown",
    do_coord_flip = TRUE,
    bar_labels = TRUE,
    bar_label_size = 4,
    bar_label_color = "blue",
    rot_y_tic_label = TRUE,
    silent_NA_warning = TRUE,
    plot_obs = TRUE,
    plot_obs_color = "darkorange",
    png_file_path = plot_file
  )
  show(a_plot)

  expect_true(is.ggplot(a_plot))
  expect_true(file.exists(plot_file))
  file.remove(plot_file)
})
