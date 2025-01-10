
test_that("create_stick_plot() aes_x aes_y", {
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
    png_file_path = tempfile()
  )
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_stick_plot() aes_x aes_y", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_stick_plot() error if aes_x or aes_y parameters are NULL",{
  expect_error(RplotterPkg::create_stick_plot(
    df = RplotterPkg::air_passengers,
    aes_x = "time"
  ))
})
