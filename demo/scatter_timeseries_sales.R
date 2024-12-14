library(here)
library(data.table)
library(magrittr)
library(RtsaPkg)
library(RplotterPkg)

# plot of US vehicle sales from time series object (ts)
sales_ts_path <- file.path(here::here(), "demo", "data", "USVsales_ts.rda")
load(sales_ts_path)

USVSales_dt <- RtsaPkg::tsObj_to_dt(series = USVSales) %>%
  data.table::setnames(old = c("time","value"), new = c("Date","USVSales"))

RplotterPkg::create_scatter_plot(
  df = USVSales_dt,
  aes_x = "Date",
  aes_y = "USVSales",
  title = "US Vehicle Sales",
  x_title = "Years",
  y_title = "Thousands of Units",
  rot_y_tic_label = TRUE,
  connect = TRUE,
  x_limits = c(as.Date("1975-01-01"), as.Date("2020-01-01")),
  x_major_breaks = seq.Date(from = as.Date("1975-1-1"), to = as.Date("2020-1-1"), by = "5 year"),
  x_date_labels = "%Y",
  y_major_breaks = seq(from = 600, to = 2000, by = 200)
)
