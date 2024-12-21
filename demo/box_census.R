library(usmap)
library(data.table)
library(ggplot2)
library(ggrepel)
library(RcensusPkg)
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
  vars = c("B19013_001E","B01002_001E","B19013_001M","B01002_001M"),
  region = "county:*",
  regionin = paste0("state:", ga_fips),
)

# Do some minor wrangling:
# Select columns
ga_dt <- ga_dt[, .(NAME,GEOID,B19013_001E,B01002_001E)]
# Reshape from wide to long
ga_long_dt <- RcensusPkg::wide_to_long(
  dt = ga_dt
)
# Redefine values in "variable" column
# Set "estimate" column as numeric
ga_inc_age_dt <- ga_long_dt[, variable := fifelse(variable == "B19013_001E", "medinc", "medage")] |>
  _[, estimate := as.numeric(estimate)]

# Reshape from long to wide form
# Split "NAME" to "County" and "State" columns
ga_wide_dt <- RcensusPkg::long_to_wide(
  dt = ga_inc_age_dt,
  parameter_col = "variable",
  value_col = "estimate"
) |>
  _[, c("County", "State") := data.table::tstrsplit(NAME, ",")]

# Plot the median income in a box plot:
RplotterPkg::create_box_plot(
  df = ga_wide_dt,
  aes_y = "medinc",
  aes_label = "County",
  y_scientific = FALSE,
  rot_y_tic_label = TRUE,
  title = "Median Household Income Among Georgia Counties",
  subtitle = "Source: Census Bureau 2015-2019 ACS (acs/acs5)",
  y_title = "Median Income ($)"
)
