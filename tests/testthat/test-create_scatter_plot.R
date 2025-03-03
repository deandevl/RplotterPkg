test_that("create_scatter_plot() namespaces", {
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_scatter_plot() aes_x aes_y", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_scatter_plot(
      df = ggplot2::economics,
      aes_x = "date",
      aes_y = "unemploy",
      pts_shape = 21,
      pts_fill = "black",
      line_color = "violet",
      connect = TRUE,
      title = "US Monthly Unemployment",
      subtitle = "July, 1967 to April, 2015 (in thousands)",
      x_title = "Date",
      y_title = "Unemployment",
      rot_y_tic_label = TRUE,
      x_date_labels = "%Y-%b",
      x_major_date_breaks = "5 year",
      y_limits = c(0, 16000),
      y_major_breaks = seq(0, 16000, 2000),
      show_minor_grids = F,
      bold_y = 8000,
      bold_y_color = "red",
      bold_y_linetype = "dashed"
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_scatter_plot() aes_x aes_y", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_scatter_plot() aes_x aes_y aes_fill",{
  expect_snapshot({
    a_plot <- RplotterPkg::create_scatter_plot(
      df = RplotterPkg::sleep,
      aes_x = "Observation",
      aes_y = "Hours",
      aes_fill = "Drug",
      title = "Increase in Hours of Sleep",
      subtitle = "Source: datasets::sleep",
      x_title = "Observation",
      y_title = "Hours",
      x_major_breaks = seq(1,20),
      y_major_breaks = seq(-2, 8, 1),
      rot_y_tic_label = TRUE,
      pts_size = 2.0
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_scatter_plot() aes_x aes_y aes_fill", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_scatter_plot() aes_x aes_y aes_label",{
  expect_snapshot({
    a_plot <- RplotterPkg::create_scatter_plot(
      df = RplotterPkg::states,
      aes_x = "Income",
      aes_y = "Murder",
      aes_label = "State",
      aes_label_color = "brown",
      aes_label_size = 4,
      aes_label_nudge_x = 0.5,
      title = "Income & Murder in the Southeast",
      subtitle = "Source: datasets::states",
      x_title = "Income",
      y_title = "Murder Rate"
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_scatter_plot() aes_x aes_y aes_label", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
