library(gt)
library(data.table)
library(purrr)
library(glue)
library(here)
library(RplotterPkg)

# ---------------------Infant Mortality-----------------------
# Read infant mortality rate (number who die for each 1000 births)
#   for 62 countries, years 2005 to 2010.
data_path <- file.path(here(), "demo", "data", "mortality_rates.txt")
mortality_dt <- data.table::fread(data_path)

percentile_lst <-RplotterPkg::percentile_table(
  vals = mortality_dt$Rate
)
# View the table
percentile_lst$table_gt

# ------------------------Values From Random Normal Distribution-----------------
set.seed(12345)
random_vals <- stats::rnorm(n = 1000, mean = 20, sd = 4)

# Plot a boxplot of the data
RplotterPkg::create_box_plot(
  df = data.table(random_vals = random_vals),
  aes_y = "random_vals",
  y_limits = c(8, 40),
  y_major_breaks = seq(8, 40, 2)
)

# Compute percentiles
percentile_random_lst <- RplotterPkg::percentile_table(
  vals = random_vals
)
# View the table
percentile_random_lst$table_gt

