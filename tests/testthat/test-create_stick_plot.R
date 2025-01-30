test_that("create_stick_plot() namespaces", {
  expect_true(requireNamespace("methods", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_stick_plot() aes_x aes_y", {
  expect_snapshot({
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
      bold_y = 0.0
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_stick_plot() aes_x aes_y", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
