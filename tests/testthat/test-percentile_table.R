
test_that("percentile_table()", {
  set.seed(12345)
  random_vals <- stats::rnorm(n = 1000, mean = 20, sd = 4)

  # Compute percentiles
  percentile_random_lst <- RplotterPkg::percentile_table(
    vals = random_vals
  )

  expect_identical(percentile_random_lst$dt$label, c("M", "F", "E", "D", "C", "B", "A"))
})

test_that("percentile_table() errors if input 'vals' is NULL",{
  expect_error(RplotterPkg::percentile_table(vals = NULL))
})
