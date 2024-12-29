
test_that("The required dataframe/aes_x/aes_y parameters are submitted and
  a .png file of the plot is created.", {

  plot_file <- "stick_plot.png"

  a_plot <- RplotterPkg::create_stick_plot(
    df = RplotterPkg::air_passengers,
    aes_x = "time",
    aes_y = "value",
    title = "Monthly Totals of International Airline Passengers",
    subtitle = "1949 - 1960 (classic Box & Jenkins)",
    x_title = "Time",
    y_title = "Totals",
    x_major_date_breaks = "1 year",
    x_date_labels = "%Y",
    rot_y_tic_label = TRUE,
    show_minor_grids = FALSE,
    bold_y = 0.0,
    png_file_path = plot_file
  )
  show(a_plot)

  expect_true(is.ggplot(a_plot))
  expect_true(file.exists(plot_file))
  file.remove(plot_file)
})
