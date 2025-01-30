test_that("create_histogram_plot() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_histogram_plot() aes_x", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_histogram_plot(
      df = RplotterPkg::midwest,
      aes_x = "Area",
      binwidth = 0.01,
      x_limits = c(0.0, 0.11),
      x_major_breaks = seq(0.0, 0.11, 0.01),
      title = "Distribution of area",
      subtitle = "437 counties from midwest dataset",
      x_title = "Area",
      y_title = "Count",
      bar_color = "white",
      bar_lwd = 2.0,
      bar_fill = "brown",
      do_coord_flip = TRUE,
      bar_labels = TRUE,
      bar_label_size = 4,
      bar_label_color = "blue",
      rot_y_tic_label = TRUE,
      silent_NA_warning = TRUE,
      plot_obs = TRUE,
      plot_obs_color = "darkorange"
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_histogram_plot() aes_x", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_histogram_plot() aes_x aes_fill aes_color", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_histogram_plot(
      df = datasets::InsectSprays,
      aes_x = "count",
      aes_color = "spray",
      aes_fill = "spray",
      title = "Counts of Insects from Insecticide Treatments",
      x_title = "Insect Counts",
      y_title = "Treatment Count",
      position = "dodge",
      bin_breaks = seq(from = 0, to = 30, by = 5),
      x_major_breaks = seq(from = 0, to = 30, by = 5),
      y_limits = c(0, 12),
      y_major_breaks = seq(from = 0, to = 12, by = 2)
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_histogram_plot() aes_x aes_fill aes_color", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
