library(RcensusPkg)
library(usmap)
library(data.table)
library(magrittr)
library(ggplot2)
library(rlang)
library(RplotterPkg)

# Note: The following census functions have a key argument and requires an access key issued from the Bureau.
#   Key can be submitted via the function argument or via the global environment with the
#   assignment of variable "CENSUS_KEY".

# Get the variable descriptions.
# Returns "household income in the past 12 months" and "age --!!Total"
var_desc_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs5",
  vintage = 2019,
  vars = c("B19013_001E", "B01002_001E")
)


# Call `Rcensus::get_vintage_data()`
# Get the median age and income for households across Georgia counties in 2019:
ga_fips <- usmap::fips("GA")
ga_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs5",
  vintage = 2019,
  vars = "B19013_001E",
  region = "county:*",
  regionin = paste0("state:", ga_fips),
)


# Do some minor wrangling:
ga_dt <- data.table::setnames(ga_dt, old = "B19013_001E", new = "medinc") %>% # relabel column
  .[, medinc := as.numeric(medinc)]  # set as numeric

# Plot the median income in a histogram
RplotterPkg::create_histogram_plot(
  df = ga_dt,
  aes_x = "medinc",
  title = "Median Household Income Among Georgia Counties",
  subtitle = "Source: Census Bureau 2015-2019 ACS (acs/acs5)",
  x_title = "Median Income ($)",
  y_title = "Count",
  rot_y_tic_label = TRUE,
  bar_fill = "green",
  bar_color = "green",
  bar_alpha = 0.5,
  bar_labels = TRUE,
  bins = 15,
  x_major_breaks = seq(20000, 120000, 20000),
  x_decimals = 0
)
