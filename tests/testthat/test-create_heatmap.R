
test_that("create_heatmap() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("methods", quietly = TRUE))
  expect_true(requireNamespace("RColorBrewer", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_heatmap() aes_x aes_y aes_fill aes_label", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_heatmap(
      df = RplotterPkg::spinrates,
      aes_x = "velocity",
      aes_y = "spinrate",
      aes_fill = "swing_miss",
      aes_label = "swing_miss",
      label_fontface = "bold",
      title = "Likelihood of swinging and missing on a fastball",
      x_title = "Velocity",
      y_title = "Spinrate",
      label_sz = 5,
      rot_y_tic_label = TRUE
    ) +
    ggplot2::scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
      n.breaks = 8
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        ticks.colour = "black"
      )
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_heatmap() aes_x aes_y aes_fill aes_label", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
