
test_that("create_scatter_plot() aes_x aes_y", {
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
    bold_y_linetype = "dashed",
    png_file_path = tempfile()
  )
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_scatter_plot() aes_x aes_y", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_scatter_plot() aes_x aes_y aes_fill",{
  sleep_dt <- data.table::as.data.table(datasets::sleep) |>
    _[, Observation := seq(1,20)] |>
    data.table::setnames(old = c("extra","group"), new = c("Hours","Drug"))

  a_plot <- RplotterPkg::create_scatter_plot(
    df = sleep_dt,
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

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_scatter_plot() aes_x aes_y aes_fill", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_scatter_plot() aes_x aes_y aes_label",{
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

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_scatter_plot() aes_x aes_y aes_label", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_scatter_plot() error if aes_x aes_y parameters are NULL",{
  expect_error(RplotterPkg::create_scatter_plot(
    df = ggplot2::economics,
    aes_x = "date"
  ))
})
