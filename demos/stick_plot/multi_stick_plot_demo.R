library(data.table)
library(ggplot2)
library(rlang)
library(grid)
library(gtable)
library(TSstudio)
library(tidyr)
library(dplyr)
library(RtsaPkg)
library(RplotterPkg)

TSstudio::ts_info(TSstudio::USUnRate)
TSstudio::ts_info(TSstudio::USVSales)

USUnRate_df <- RtsaPkg::ts_to_df(TSstudio::USUnRate) %>%
  filter(DateTime > as.Date("1975-12-30")) %>%
  rename(USUnRate = Value)

USVSales_df <- RtsaPkg::ts_to_df(TSstudio::USVSales) %>%
  rename(USVSales = Value)

USUnRate_USVSales_df <- dplyr::inner_join(USUnRate_df, USVSales_df)

USUnRate_USVSales_df <- USUnRate_USVSales_df %>%
  tidyr::pivot_longer(cols = -DateTime, names_to = "source", values_to = "source_data")
str(USUnRate_USVSales_df)

USUnRate_USVSales_plot <- RplotterPkg::multi_stick_plot(
  df = USUnRate_USVSales_df,
  factor_var = "source",
  factor_x = "DateTime",
  columns = 1,
  aes_y = "source_data",
  title = "Monthly US Unemployment Rate vs US Vehicle Sales",
  subtitle = "1976-2020",
  y_titles = c("Rate", "Count"),
  col_width = 8
)
