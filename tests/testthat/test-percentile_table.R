test_that("percentile_table() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("gt", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("percentile_table()", {
  expect_snapshot({
    set.seed(12345)
    random_vals <- stats::rnorm(n = 1000, mean = 20, sd = 4)

    # Compute percentiles
    percentile_random_lst <- RplotterPkg::percentile_table(
      vals = random_vals
    )
  })
  expect_identical(percentile_random_lst$dt$label, c("M", "F", "E", "D", "C", "B", "A"))
})
