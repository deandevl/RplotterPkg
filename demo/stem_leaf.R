library(data.table)
library(here)
library(magrittr)
library(aplpack)
library(RplotterPkg)

# read the Boston marathon ages/times for women
data_path <- file.path(here(), "demo", "data", "boston_marathon.txt")
marathon_dt <- data.table::fread(data_path) %>%
  na.omit(.)

# stem and leaf for marathon times of women across ages
marathon_times_lst <- list(
  "age_20" = marathon_dt[age == 20,]$time,
  "age_30" = marathon_dt[age == 30,]$time,
  "age_40" = marathon_dt[age == 40,]$time,
  "age_50" = marathon_dt[age == 50,]$time,
  "age_60" = marathon_dt[age == 60,]$time
)
# display stem and leaf of women times across ages
RplotterPkg::stem_leaf_display(
  x = marathon_times_lst,
  heading_color = "#FF5500"
)

# display again showing cumulative depth of leaves
RplotterPkg::stem_leaf_display(
  x = marathon_times_lst,
  depths = TRUE,
  col_width = 3,
  font_sz = 8,
  heading_color = "#FF5500"
)
