
test_that("create_density_ridge_plot()", {

  variables_v <- c("HS_Diploma", "College_Edu", "Prof_Deg", "White", "Black", "Asian")

  a_plot <- RplotterPkg::create_density_ridge_plot(
    df = RplotterPkg::midwest,
    bw = "sj",
    variables = variables_v,
    title = "Percent Distribution Among Midwest Counties",
    x_limits = c(0, 100),
    x_major_breaks = seq(0, 100, 10),
    density_fill = "blue",
    density_alpha = 0.5
  )

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_density_ridge_plot()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})

test_that("create_density_ridge_plot() error if variables parameter is NULL",{
  expect_error(RplotterPkg::create_density_ridge_plot(
    df = RplotterPkg::midwest
  ))
})
