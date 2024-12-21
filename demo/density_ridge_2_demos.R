library(rlang)
library(grid)
library(gtable)
library(ggplot2)
library(data.table)
library(usmap)
library(RcensusPkg)
library(RplotterPkg)

# -----------First demo: Ridge plot densities for midwest variables
dt <- data.table::setDT(ggplot2::midwest) |>
  _[, .(perchsd, percollege, percprof, percwhite, percblack, percasian)] |>
  data.table::setnames(
    old = c("perchsd", "percollege", "percprof", "percwhite", "percblack", "percasian"),
    new = c("HS Diploma", "College Edu", "Prof Deg", "White", "Black", "Asian")
  )

RplotterPkg::create_density_ridge_plot(
  df = dt,
  bw = "sj",
  variables = c("HS Diploma", "College Edu", "Prof Deg", "White", "Black", "Asian"),
  title = "Percent Distribution Among Midwest Counties",
  x_limits = c(0, 100),
  x_major_breaks = seq(0, 100, 10),
  density_fill = "blue",
  density_alpha = 0.5
)

# ----------------Second demo: Census Bureau median home values for select counties in Oregon
# Note: The following census related functions have a key argument and requires an access key issued from the Bureau.
#   Key can be submitted via the function argument or via the global environment with the
#   assignment of variable "CENSUS_KEY".

# Get description of Census variable "B25077_001E"
var_desc_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs5",
  vintage = 2019,
  vars = "B25077_001E"
)

# Get fips values for Oregon counties of interest
or_fips <- usmap::fips(state = "OR")
multnomah_fips <- substr(usmap::fips(state = "OR", county = "Multnomah"),3,5)
clackamas_fips <- substr(usmap::fips(state = "OR", county = "Clackamas"),3,5)
washington_fips <-substr(usmap::fips(state = "OR", county = "Washington"),3,5)
yamhill_fips <- substr(usmap::fips(state = "OR", county = "Yamhill"),3,5)
marion_fips <-substr(usmap::fips(state = "OR", county = "Marion"),3,5)
columbia_fips <-substr(usmap::fips(state = "OR", county = "Columbia"),3,5)

tract_fips <- c(multnomah_fips, clackamas_fips, washington_fips, yamhill_fips, marion_fips, columbia_fips)

# Get the home values
home_vals_dt <- NULL
for(a_fips in tract_fips){
  tracts_vals_dt <- RcensusPkg::get_vintage_data(
    dataset = "acs/acs5",
    vintage = 2020,
    vars = list(estimate = "B25077_001E", moe = "B25077_001M"),
    region = "tract:*",
    regionin = paste0("state:",or_fips,"+county:",a_fips)
  )
  home_vals_dt <- rbind(home_vals_dt, tracts_vals_dt)
}

# Do some simple data wrangling
home_vals_dt <- home_vals_dt[order(county,tract)] |>   # order by county, tract numbers
  _[, c("tract_str", "county_str", "state_str") := tstrsplit(NAME, ",", fixed = TRUE)] |>    # split NAME string into tract,county,state
  data.table::setnames(old = c("B25077_001E", "B25077_001M"), new = c("estimate", "moe")) |>  # rename columns
  _[, estimate := as.numeric(estimate)] |>   # set as numeric
  _[estimate > 0] |>    # filter rows for only positive values
  _[, list(GEOID, state_str, estimate), by = county_str]  # select variables, aggregate


# Reshape home_vals_dt to a "wide" format
home_vals_wide_dt <- RcensusPkg::long_to_wide(
  dt = home_vals_dt,
  id_v = c("GEOID", "state_str"),
  parameter_col = "county_str",
  value_col = "estimate"
)

# Rename the columns of home_vals_wide_dt
data.table::setnames(home_vals_wide_dt,
                     old = c(" Clackamas County", " Columbia County", " Marion County", " Multnomah County", " Washington County", " Yamhill County"),
                     new = c("Clackamas", "Columbia", "Marion", "Multnomah", "Washington", "Yamhill"))

RplotterPkg::create_density_ridge_plot(
  df = home_vals_wide_dt,
  variables = names(home_vals_wide_dt)[3:8],
  title = "Median Home Values Across Selected Oregon Counties",
  y_show_axis = TRUE,
  plot_heights = 3.0,
  plot_widths = 30.0
)
