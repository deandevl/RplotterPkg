
test_that("Reading data for the required 'df' parameter and testing returned list", {

  spread_level_lst <- RplotterPkg::spread_level_plot(
    df = RplotterPkg::homeruns_2000,
    meas_var = "HOMERUNS",
    factor_var = "YEARS",
    x_title = "Log Median",
    y_title = "Log Spread"
  )

  show(spread_level_lst$scatter_plot)

  expect_identical(names(spread_level_lst), c("df","spread_level_lm","scatter_plot"))
})
