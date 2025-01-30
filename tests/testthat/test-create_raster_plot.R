test_that("create_raster_plot() namespaces", {
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_raster_plot() aes_x aes_y aes_fill", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_raster_plot(
      df = RplotterPkg::kentucky_elevation,
      title = "County Elevations in Southeast Kentucky",
      aes_x = "x",
      aes_y = "y",
      aes_fill = "elevation"
    ) +
    ggplot2::geom_sf(
      data = RplotterPkg::kentucky_counties,
      aes(x = NULL, y = NULL),
      alpha = 0,
      linewidth = 1.5
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_raster_plot() aes_x aes_y aes_fill", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
