library(data.table)
library(magrittr)
library(rlang)
library(ggplot2)
library(forcats)
library(lubridate)
library(RColorBrewer)
library(here)
library(RplotterPkg)

# Source of data is US Census Bureau's "Business and Industry" page
#  that provides a downloadable csv file for new home sales. The data is
#  formatted in a matrix of sale values for years along the rows; months
#  along the columns.

# In the csv file:
# The dates are defined starting at row 42 to 729 "TIME PERIODS"
# The data are defined starting at row 741 to 24807 "DATA"

# Define the file path to the data
sales_data_file_path <- file.path(
  here::here(),
  "demo",
  "data",
  "RESSALES-mf.csv"
)

# Read the data
# cat_idx: 1 = housing sales; 3 = housing supply
# dt_idx: 1 = all houses
# geo_idx: 1 = just usa
# is_adj: 1 = seasonally adjusted
dt <- data.table::fread(
  file = sales_data_file_path,
  skip = 740,
  key = "per_idx"
)
str(dt)

# Set a data.table with 687 dates to be binded to sales_dt
dates_dt <- data.table::data.table(
  per_idx = 1:687,
  date=seq.Date(from = as.Date("1963-01-01"), by = "1 month", length.out = 687)) %>%
  data.table::setkey(., "per_idx")
str(dates_dt)

# Bind dt with dates_dt with the common key "per_idx"
data_dt <- dt[dates_dt]

# We will be looking at housing sales where "cat_idx" equals 1.
# Get housing sales and select "val" and "date"
sales_dt <- data_dt[cat_idx == 1 & dt_idx == 1 & geo_idx == 1, .(val, date)] %>%
  data.table::setnames(., old = "val", new = "Sales")
str(sales_dt)

# Add month and year columns
sales_dt[, `:=`(
  month_factor = forcats::fct_reorder(as.character(lubridate::month(date), format = "%b"), lubridate::month(date)),
  year_factor = forcats::fct_reorder(as.factor(lubridate::year(date)), -lubridate::year(date))
)]
str(sales_dt)

# Filter to just years > 1999
sales_gt1999_dt <- sales_dt[lubridate::year(date) > 1999,]

# Plot the data in a heatmap
RplotterPkg::create_heatmap(
  df = sales_gt1999_dt,
  aes_x = "month_factor",
  aes_y = "year_factor",
  aes_fill = "Sales",
  aes_label = "Sales",
  label_color = "black",
  title = "U.S. New Home Sales by Month",
  subtitle = "1000's of homes not seasonally adjusted",
  caption = "Source: U.S. Census Bureau and U.S. Department of Housing and Urban Developmet",
  x_title = "Month",
  y_title = "Year",
  rot_y_tic_label = T
) +
ggplot2::scale_fill_gradientn(
  colors = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
  n.breaks = 8
) +
ggplot2::guides(
  fill = ggplot2::guide_colorbar(
    ticks.colour = "black"
  )
)
