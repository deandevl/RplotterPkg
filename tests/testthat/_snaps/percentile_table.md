# percentile_table()

    Code
      set.seed(12345)
      random_vals <- stats::rnorm(n = 1000, mean = 20, sd = 4)
      percentile_random_lst <- RplotterPkg::percentile_table(vals = random_vals)

