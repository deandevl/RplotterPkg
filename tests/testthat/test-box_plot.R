
test_that("The required dataframe/aes_y parameters are submitted and
  a .png file of the plot is created.", {

  plot_file <- "box_plot.png"

  a_plot <- RplotterPkg::create_box_plot(
    df = RplotterPkg::organdata,
    aes_x = "country",
    aes_y = "donors",
    aes_label = "donors",
    aes_label_color = "red",
    order_by_median = "desc",
    y_limits = c(5,35),
    y_major_breaks = seq(5, 35, 5),
    title = "Organ Donation Rate per Million",
    subtitle = "Showing outlier rates",
    x_title = "Country",
    y_title = "Donor Rate",
    box_color = "purple",
    box_line_width = 0.8,
    rot_x_tic_angle = 40,
    rot_y_tic_label = TRUE,
    ol_color = "red",
    ol_size = 1.5,
    silent_NA_warning = TRUE,
    png_file_path = plot_file
  )
  show(a_plot)

  expect_true(is.ggplot(a_plot))
  expect_true(file.exists(plot_file))
  file.remove(plot_file)
})
