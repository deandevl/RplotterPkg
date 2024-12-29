
test_that("The required parameters 'df` is a dataframe and the returned object
  is of class 'gtable'", {

  variables_v <- c("HS_Diploma", "College_Edu", "Prof_Deg", "White", "Black", "Asian")

  a_plot <- RplotterPkg::create_density_ridge_plot(
    df = RplotterPkg::midwest,
    bw = "sj",
    variables = variables_v,
    title = "Percent Distribution Among Midwest Counties",
    x_limits = c(0, 100),
    x_major_breaks = seq(0, 100, 10),
    density_fill = "blue",
    density_alpha = 0.5,
    display_plot = TRUE
  )
  show(a_plot)

  expect_true(gtable::is.gtable(a_plot))
})
