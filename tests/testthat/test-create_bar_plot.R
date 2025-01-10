
test_that("create_bar_plot() aes_x aes_fill", {
  a_plot <- RplotterPkg::create_bar_plot(
    df = RplotterPkg::religion,
    aes_x = "happy",
    aes_fill = "religion",
    position = "dodge",
    title = "Happy Religions",
    center_titles = TRUE,
    rot_y_tic_label = TRUE,
    bar_width = 0.8,
    order_bars = "desc",
    x_title = "Happiness",
    y_title = "Count",
    axis_text_size = 16,
    png_file_path = tempfile()
  )
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_bar_plot() aes_x aes_fill", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_bar_plot() aes_x aes_y aes_fill aes_color", {
  library(data.table)

  chick_weights_dt <- data.table::as.data.table(datasets::ChickWeight) |>
    _[Time == 21,
    lapply(.SD, mean),
    by = Diet,
    .SDcols = c("weight")
  ] |>
  _[, Label := c("10%_Pro","20%_Pro","40%_Pro","60%_Pro")] |>
  _[, weight := round(weight, digits = 0)]

  a_plot <- RplotterPkg::create_bar_plot(
    df = chick_weights_dt,
    aes_x = "Label",
    aes_y = "weight",
    aes_fill = "Diet",
    aes_color = "Diet",
    bar_labels = TRUE,
    x_title = "Chick Diet",
    y_title = "Chick Weight",
    order_bars = "asc"
  )

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_bar_plot() aes_x aes_y aes_fill aes_color", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_bar_plot() major_breaks aes_x",{
  a_plot <- RplotterPkg::create_bar_plot(
    df = datasets::ToothGrowth,
    aes_x = "len",
    x_major_breaks = seq(from = 0, to = 40, by = 5),
    y_major_breaks = seq(from = 0, to = 16, by = 2),
    y_limits = c(0, 16),
    bar_labels = TRUE,
    bar_fill = "blue",
    bar_alpha = 0.6,
    title = "Tooth Growth",
    subtitle = "source: datasets::ToothGrowth",
    x_title = "Length",
    y_title = "Count"
  )

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_bar_plot() major_breaks aes_x", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_bar_plot() major_breaks do_coord_flip aes_x", {
  a_plot <- RplotterPkg::create_bar_plot(
    df = datasets::ToothGrowth,
    aes_x = "len",
    x_major_breaks = seq(from = 0, to = 40, by = 5),
    y_major_breaks = seq(from = 0, to = 16, by = 2),
    y_limits = c(0, 16),
    bar_labels = TRUE,
    bar_fill = "blue",
    bar_alpha = 0.6,
    do_coord_flip = TRUE,
    rot_y_tic_label = TRUE,
    title = "Tooth Growth",
    subtitle = "source: datasets::ToothGrowth",
    x_title = "Length",
    y_title = "Count"
  )

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_bar_plot() major_breaks do_coord_flip aes_x", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_bar_plot() error if df parameter is NULL",{
  expect_error(RplotterPkg::create_bar_plot())
})
test_that("create_bar_plot() error if aes_x parameter is NULL",{
  expect_error(RplotterPkg::create_bar_plot(
    df = RplotterPkg::religion)
  )
})
