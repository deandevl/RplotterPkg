#' @title air_passengers data
#'
#' @description
#' Data where the base \code{datasets::AirPassengers} monthly time
#'   series of international airline passenger numbers 1949-1960 was converted to
#'   a data frame using \code{tsbox::ts_df(datasets::AirPassengers)}.
#'
#' \tabular{ll}{
#'   \strong{time} \tab Date with format 1949-01-01 \cr
#'   \strong{value} \tab numeric number of passengers in thousands
#' }
#' @source \code{datasets::AirPassengers}
#'
"air_passengers"

#' @title
#' boston_marathon data
#'
#' @description
#' Boston Marathon completion times of women of different ages. The
#' raw tab delimited data (\code{boston_marathon.txt}) is read by
#' \code{data.table::fread()} to produce a data frame with columns
#' for women's "age" and marathon "time" in minutes.
#'
#' \tabular{ll}{
#'   \strong{age} \tab numeric age from 20 to 60 \cr
#'   \strong{time} \tab numeric marathon time
#' }
#' @source \href{https://github.com/bayesball/LearnEDAfunctions}{LearnEDAfunctions}
#'
"boston_marathon"

#' @title
#' car_stats data
#'
#' @description
#' From \code{datasets::mtcars} produces statistics for N and the means of
#' "mpg", "hp", "wt", "disp" variables grouped by "cyl".
#'
#' \tabular{ll}{
#'   \strong{N} \tab numeric of group's total observations \cr
#'   \strong{mean_mpg} \tab numeric of group's mean mpg \cr
#'   \strong{mean_hp} \tab numeric of group's mean horsepower \cr
#'   \strong{mean_wt} \tab numeric of group's mean weight \cr
#'   \strong{mean_disp} \tab numeric of group's mean displacment
#' }
#' @source \code{datasets::mtcars}
#'
"car_stats"

#' @title
#' farms data
#'
#' @description
#' Number of farms in the states of the U.S. The
#' raw tab delimited data (\code{farms.txt}) is read by
#' \code{data.table::fread()} to produce a data frame with columns
#' for "state" and "count".
#'
#' \tabular{ll}{
#'   \strong{state} \tab character as a state abbreviation \cr
#'   \strong{count} \tab numeric of state's total farm count
#' }
#' @source \href{https://github.com/bayesball/LearnEDAfunctions}{LearnEDAfunctions}
#'
"farms"

#' @title
#' gapminder_data
#'
#' @description
#' Based on \code{gapminder::gapminder} dataset, selects 10 year intervals
#' and 4 continents.  The "year" variable is defined as a factor and the "pop"
#' variable is divided by 1e+6.
#'
#' \tabular{ll}{
#'   \strong{year} \tab numeric factor as 1952, 1972, 1992, 2002 \cr
#'   \strong{pop} \tab population in units of 1e+6 \cr
#'   \strong{continent} \tab factor with 4 levels \cr
#'   \strong{lifeExp} \tab life expectancy at birth, in years
#' }
#' @source \code{gapminder::gapminder}
#'
"gapminder_data"

#' @title
#' homeruns_2000 data
#'
#' @description
#' Team home run numbers for different seasons. The
#' raw tab delimited data (\code{homeruns_2000.txt}) is read by
#' \code{data.table::fread()} to produce a data frame with numeric
#' columns for "YEARS" and "HOMERUNS".
#'
#' \tabular{ll}{
#'   \strong{YEARS} \tab numeric years for baseball teams from 1900 to 2000 \cr
#'   \strong{HOMERUNS} \tab numeric of total homeruns for a baseball team
#' }
#' @source \href{https://github.com/bayesball/LearnEDAfunctions}{LearnEDAfunctions}
#'
"homeruns_2000"

#' @title
#' midwest data
#'
#' @description
#' From the \code{ggplot2::midwest} dataset, selects for midwest counties
#' percent high school diploma, percent college educated, percentage professional
#' degree, percent white, percent black, percent asian.
#'
#' \tabular{ll}{
#'   \strong{HS_Diploma} \tab numeric percent high school diploma \cr
#'   \strong{College_Edu} \tab numeric percent college educated \cr
#'   \strong{Prof_Deg} \tab percentage professional degree \cr
#'   \strong{White} \tab percent white \cr
#'   \strong{Black} \tab percent black \cr
#'   \strong{Asian} \tab percent asian
#' }
#' @source \code{ggplot2::midwest}
#'
"midwest"

#' @title
#' organdata data
#'
#' @description
#' From the \code{socviz::organdata} dataset two variables are
#' selected: Organ Donation rate per million population and
#' country.
#'
#' \tabular{ll}{
#'   \strong{country} \tab character country name \cr
#'   \strong{donors} \tab Organ Donation rate per million population
#' }
#' @source \code{socviz::organdata}
#'
"organdata"

#' @title
#' penguins_stats data
#'
#' @description
#' The \code{palmerpenguins:penguins} dataset is grouped by the variable
#' "species" and a data frame is produced with statistics for
#' average, minimum and maximum body mass.
#'
#' \tabular{ll}{
#'   \strong{avg_body_mass} \tab numeric species average body mass \cr
#'   \strong{min_body_mass} \tab numeric species minimum body mass \cr
#'   \strong{max_body_mass} \tab numeric species maximum body mass \cr
#'   \strong{mid_labels} \tab character species average body mass
#' }
#' @source \code{palmerpenguins:penguins}
#'
"penguins_stats"

#' @title
#' people data
#'
#' @description
#' A made-up data frame with generic column names and mix of
#' numeric and character data.
#'
#' \tabular{ll}{
#'   \strong{col_0} \tab numeric random numbers for ids \cr
#'   \strong{col_1} \tab character first names \cr
#'   \strong{col_2} \tab character last names \cr
#'   \strong{col_3} \tab numeric ages \cr
#'   \strong{col_4} \tab character addresses \cr
#'   \strong{col_5} \tab character spouse's first name
#' }
#' @source R coding with \code{data.table::data.table()}
#'
"people"

#' @title
#' religion data
#'
#' @description
#' From the \code{socviz::gss_sm} dataset General Social Survey, 2016,
#' a sub-data frame was created from the "happy" and "religion" variables.
#'
#' \tabular{ll}{
#'   \strong{happy} \tab general happiness \cr
#'   \strong{religion} \tab religion in six categories
#' }
#' @source \code{socviz::gss_sm}
#'
"religion"

#' @title
#' spinrates data
#'
#' @description
#' From the raw data file \code{spinrates.csv} the comma delimited data
#' was converted to a data frame using \code{data.table::fread()}.
#' The data are baseball percent "swing and miss" with their respective average
#' ball velocity and levels of spinrate.
#'
#' \tabular{ll}{
#'   \strong{id} \tab character giving the observation id \cr
#'   \strong{velocity} \tab numeric average velocity of the ball \cr
#'   \strong{spinrate} \tab character factor with 12 levels \cr
#'   \strong{swing_miss} \tab numeric rate of swing and miss \cr
#' }
#' @source \href{https://www.fangraphs.com}{Fangraphs}
#'
"spinrates"

#' @title
#' world_coffee data
#'
#' @description
#' World coffee production by country (in thousands of 60 kg bags)
#'   is joined with a simple feature of country geometries via their
#'   common parameter 'name_long'.
#'
#' \tabular{ll}{
#'   \strong{name_long} \tab character long name of country \cr
#'   \strong{coffee_production_2017} \tab numeric of coffee production of 60 kg bags \cr
#'   \strong{geom} \tab the sfc_MULTIPOLYGON country geometries
#' }
#' @source \code{spData::coffee_data} and \code{spData::world}
#'
"world_coffee"

#' @title
#' kentucky_counties data
#'
#' @description
#' A simple feature object with the boundary geometries for nine
#' southeast Kentucky counties.
#' .
#'
#' \tabular{ll}{
#'   \strong{ID} \tab character giving the name of the county \cr
#'   \strong{geom} \tab MULTIPOLYGON of the Kentucky county boundary
#' }
#' @source \code{maps::map()}
#'
"kentucky_counties"

#' @title
#' kentucky_elevation data
#'
#' @description
#' Tiled raster elevation data(in meters) converted to a data frame,
#' covering the southeast region of Kentucky.
#'
#' \tabular{ll}{
#'   \strong{x} \tab numeric longitude of the elevation \cr
#'   \strong{y} \tab numeric latitude of the elevation \cr
#'   \strong{elevation} \tab the elevation in meters
#' }
#' @source \code{elevatr::get_elev_raster()} and the
#' Amazon Web Services Terrain Tiles service.
#'
"kentucky_elevation"

#' @title
#' states data
#'
#' @description
#' Consolidation of data into one dataframe for the "South" region.
#'
#' \tabular{ll}{
#'   \strong{State} \tab character state name \cr
#'   \strong{Income} \tab numeric state income \cr
#'   \strong{Murder} \tab numeric state murder rate
#' }
#' @source \code{datasets::state_name, state_region, state_x77}
#'
"states"

#' @title
#' chick_weights data
#'
#' @description
#' A dataframe with 4 observations of average chick weights.
#'
#' \tabular{ll}{
#'  \strong{Diet} \tab numeric diet index \cr
#'  \strong{Weight} \tab numeric of average chick's weight \cr
#'  \strong{Label} \tab string label for the diet in percent of protein
#' }
#' @source \code{datasets::ChickWeight}
"chick_weights"

#' @title
#' sleep data
#'
#' @description
#' A dataframe with 20 observations that shows effect of two soporific
#' drugs (increase in hours of sleep compared to control) on 10 patients.
#'
#' \tabular{ll}{
#'   \strong{Hours} \tab extra numeric increase in hours of sleep \cr
#'   \strong{Drug} \tab factor with 2 levels of drug groups (1,2) \cr
#'   \strong{ID} \tab factor with 10 levels of patient ID (1 to 10) \cr
#'   \strong{Observation} \tab numeric observation id (1 to 20)
#' }
#' @source \code{datasets::sleep}
 "sleep"
