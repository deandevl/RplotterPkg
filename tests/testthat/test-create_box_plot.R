
test_that("create_box_plot() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_box_plot() aes_y aes_label",{
  expect_snapshot({
    set.seed(224)
    air_df <- datasets::airquality |>
      na.omit()
    a_plot <- RplotterPkg::create_box_plot(
      df = air_df,
      aes_y = "Ozone",
      aes_label = "Month",
      ol_color = "red",
      ol_size = 1.5,
      title = "Air Quality",
      subtitle = "Showing outlier months July(7), August(8)",
      y_title = "Ozone (ppb)",
      box_fill = "brown",
      box_alpha = 0.7,
      y_limits = c(0, 200),
      y_major_breaks = seq(from = 0, to = 200, by = 20),
      rot_y_tic_label = TRUE
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_box_plot() aes_y aes_label", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_box_plot() aes_x, aes_y aes_label", {
  expect_snapshot({
    set.seed(224)
    a_plot <- RplotterPkg::create_box_plot(
      df = RplotterPkg::organdata,
      aes_x = "country",
      aes_y = "donors",
      aes_label = "donors",
      order_by_median = "desc",
      y_limits = c(5,35),
      y_major_breaks = seq(5, 35, 5),
      title = "Organ Donation Rate per Million",
      subtitle = "Showing outlier rates",
      center_titles = TRUE,
      x_title = "Country",
      y_title = "Donor Rate",
      do_coord_flip = TRUE,
      box_color = "purple",
      box_line_width = 0.8,
      rot_y_tic_label = TRUE,
      ol_color = "red",
      ol_size = 1.5
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_box_plot() aes_x, aes_y aes_label", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_box_plot() aes_y, aes_fill aes_color",{
  expect_snapshot({
    set.seed(224)
    orange_df <- datasets::Orange |>
      data.table::as.data.table() |>
      _[, Tree := factor(Tree, levels = c("1","2","3","4","5"))]

    a_plot <- RplotterPkg::create_box_plot(
      df = orange_df,
      aes_y = "circumference",
      aes_fill = "Tree",
      aes_color = "Tree",
      title = "Growth of Orange Trees",
      subtitle = "source: datasets::Orange"
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_box_plot() aes_y, aes_fill aes_color", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

