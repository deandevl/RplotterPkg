test_that("spread_level_plot() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("spread_level_plot()", {
  expect_snapshot({
    spread_level_lst <- RplotterPkg::spread_level_plot(
      df = RplotterPkg::homeruns_2000,
      meas_var = "HOMERUNS",
      factor_var = "YEARS",
      x_title = "Log Median",
      y_title = "Log Spread"
    )
  })
  expect_true(is.ggplot(spread_level_lst$scatter_plot))
  vdiffr::expect_doppelganger("spread_level_plot()", spread_level_lst$scatter_plot)
  expect_no_error(ggplot_build(spread_level_lst$scatter_plot))
})
