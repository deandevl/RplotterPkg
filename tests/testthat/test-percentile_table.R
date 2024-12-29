
test_that("The returned list has names 'dt' and 'table_gt'", {

  set.seed(12345)
  random_vals <- stats::rnorm(n = 1000, mean = 20, sd = 4)

  # Compute percentiles
  percentile_random_lst <- RplotterPkg::percentile_table(
    vals = random_vals
  )
  # View the table
  show(percentile_random_lst$table_gt)

  expect_identical(names(percentile_random_lst), c("dt", "table_gt"))
})
