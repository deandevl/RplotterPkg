
test_that("create_density_plot() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_density_plot() aes_x x_major_breaks plot_obs", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_density_plot(
      df = datasets::airquality,
      aes_x = "Ozone",
      rot_y_tic_label = TRUE,
      x_limits = c(-40,200),
      x_major_breaks = seq(-40,200,20),
      title = "Ozone Air Quality",
      x_title = "Ozone",
      y_title = "Density",
      plot_obs = TRUE,
      density_fill = "green",
      density_alpha = 0.5,
      silent_NA_warning = TRUE
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_density_plot() aes_x x_major_breaks plot_obs", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_density_plot() aes_x cum_prob=.95", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_density_plot(
      df = datasets::airquality,
      aes_x = "Ozone",
      cum_prob = 0.95,
      rot_y_tic_label = TRUE,
      x_limits = c(-40,200),
      x_major_breaks = seq(-40,200,20),
      rot_x_tic_angle = 40,
      density_fill = "green",
      density_alpha = 0.5,
      silent_NA_warning = TRUE
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_density_plot() aes_x cum_prob=.95", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_density_plot() aes_x cum_prob=1.00", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_density_plot(
      df = datasets::airquality,
      aes_x = "Ozone",
      cum_prob = 1.00,
      x_limits = c(-40,200),
      x_major_breaks = seq(-40,200,20),
      hide_x_tics = TRUE,
      hide_y_tics = TRUE,
      density_fill = "green",
      density_alpha = 0.5,
      silent_NA_warning = TRUE
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_density_plot() aes_x cum_prob=1.00", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_density_plot() aes_x cum_prob=c(0.25,0.75)", {
  expect_snapshot({
    a_plot <- RplotterPkg::create_density_plot(
      df = datasets::airquality,
      aes_x = "Ozone",
      cum_prob = c(0.25,0.75),
      x_limits = c(-40,200),
      x_major_breaks = seq(-40,200,20),
      hide_x_tics = TRUE,
      hide_y_tics = TRUE,
      density_fill = "green",
      density_alpha = 0.5,
      silent_NA_warning = TRUE
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_density_plot() aes_x cum_prob=c(0.25,0.75)", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

