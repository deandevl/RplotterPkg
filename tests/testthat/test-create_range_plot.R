
test_that("create_range_plot() namespaces", {
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_range_plot()", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_range_plot(
      df = RplotterPkg::penguins_stats,
      orientation = "x",
      aes_x = "avg_body_mass",
      aes_y = "species",
      aes_min = "min_body_mass",
      aes_max = "max_body_mass",
      title = "Average and Range of Penguins Body Mass(g) by Species",
      subtitle = "Source: palmerpenguins",
      center_titles = TRUE,
      x_title = "Body Mass(g)",
      y_title = "Species",
      pts_fill = "blue",
      pts_shape = 22,
      pts_stroke = 1.7,
      pts_size = 3,
      line_width = 1.5,
      line_type = "solid",
      line_pts_color = "red",
      line_pts_alpha = 0.5,
      x_limits = c(2500, 7000),
      x_major_breaks = seq(2500,7000,500),
      show_major_grids = TRUE,
      show_minor_grids = FALSE
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_range_plot()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
