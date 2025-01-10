#' air_passengers data
#'
#' Data where the base \code{datasets::AirPassengers} monthly time
#'   series of international airline passenger numbers 1949-1960 was converted to
#'   a data frame using \code{tsbox::ts_df(datasets::AirPassengers)}.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{time}{Date with format 1949-01-01}
#'   \item{value}{numeric number of passengers in thousands}
#' }
#' @source \code{datasets::AirPassengers}
#'
"air_passengers"

#' boston_marathon data
#'
#' Boston Marathon completion times of women of different ages. The
#' raw tab delimited data (\code{boston_marathon.txt}) is read by
#' \code{data.table::fread()} to produce a data frame with columns
#' for women's "age" and marathon "time" in minutes.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{age}{numeric age from 20 to 60}
#'   \item{time}{numeric marathon time}
#' }
#' @source \href{https://github.com/bayesball/LearnEDAfunctions}{LearnEDAfunctions}
#'
"boston_marathon"

#' car_stats data
#'
#' From \code{datasets::mtcars} produces statistics for N and the means of
#' "mpg", "hp", "wt", "disp" variables grouped by "cyl".
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{N}{numeric of group's total observations}
#'   \item{mean_mpg}{numeric of group's mean mpg}
#'   \item{mean_hp}{numeric of group's mean horsepower}
#'   \item{mean_wt}{numeric of group's mean weight}
#'   \item{mean_disp}{numeric of group's mean displacment}
#' }
#' @source \code{datasets::mtcars}
#'
"car_stats"

#' farms data
#'
#' Number of farms in the states of the U.S. The
#' raw tab delimited data (\code{farms.txt}) is read by
#' \code{data.table::fread()} to produce a data frame with columns
#' for "state" and "count".
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{state}{character as a state abbreviation}
#'   \item{count}{numeric of state's total farm count}
#' }
#' @source \href{https://github.com/bayesball/LearnEDAfunctions}{LearnEDAfunctions}
#'
"farms"

#' gapminder_data
#'
#' Based on \code{gapminder::gapminder} dataset, selects 10 year intervals
#' and 4 continents.  The "year" variable is defined as a factor and the "pop"
#' variable is divided by 1e+6.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{year}{numeric factor as 1952, 1972, 1992, 2002}
#'   \item{pop}{population in units of 1e+6}
#'   \item{continent}{factor with 4 levels}
#'   \item{lifeExp}{life expectancy at birth, in years}
#' }
#' @source \code{gapminder::gapminder}
#'
"gapminder_data"

#' homeruns_2000 data
#'
#' Team home run numbers for different seasons. The
#' raw tab delimited data (\code{homeruns_2000.txt}) is read by
#' \code{data.table::fread()} to produce a data frame with numeric
#' columns for "YEARS" and "HOMERUNS".
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{YEARS}{numeric years for baseball teams from 1900 to 2000}
#'   \item{HOMERUNS}{numeric of total homeruns for a baseball team}
#' }
#' @source \href{https://github.com/bayesball/LearnEDAfunctions}{LearnEDAfunctions}
#'
"homeruns_2000"

#' midwest data
#'
#' From the \code{ggplot2::midwest} dataset, selects for midwest counties
#' percent high school diploma, percent college educated, percentage professional
#' degree, percent white, percent black, percent asian.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{HS_Diploma}{numeric percent high school diploma}
#'   \item{College_Edu}{numeric percent college educated}
#'   \item{Prof_Deg}{percentage professional degree}
#'   \item{White}{percent white}
#'   \item{Black}{percent black}
#'   \item{Asian}{percent asian}
#' }
#' @source \code{ggplot2::midwest}
#'
"midwest"

#' organdata data
#'
#' From the \code{socviz::organdata} dataset two variables are
#' selected: Organ Donation rate per million population and
#' country.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{country}{character country name}
#'   \item{donors}{Organ Donation rate per million population}
#' }
#' @source \code{socviz::organdata}
#'
"organdata"

#' penguins_stats data
#'
#' The \code{palmerpenguins:penguins} dataset is grouped by the variable
#' "species" and a data frame is produced with statistics for
#' average, minimum and maximum body mass.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{avg_body_mass}{numeric species average body mass}
#'   \item{min_body_mass}{numeric species minimum body mass}
#'   \item{max_body_mass}{numeric species maximum body mass}
#' }
#' @source \code{palmerpenguins:penguins}
#'
"penguins_stats"

#' people data
#'
#' A made-up data frame with generic column names and mix of
#' numeric and character data.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{col_0}{numeric random numbers for ids'}
#'   \item{col_1}{character first names}
#'   \item{col_2}{character last names}
#'   \item{col_3}{numeric ages}
#'   \item{col_4}{character addresses}
#'   \item{col_5}{character spouse's first name}
#' }
#' @source R coding with \code{data.table::data.table()}
#'
"people"

#' religion data
#'
#' From the \code{socviz::gss_sm} dataset General Social Survey, 2016,
#' a sub-data frame was created from the "happy" and "religion" variables.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{happy}{general happiness}
#'   \item{religion}{religion in six categories}
#' }
#' @source \code{socviz::gss_sm}
#'
"religion"

#' spinrates data
#'
#' From the raw data file \code{spinrates.csv} the comma delimited data
#' was converted to a data frame using \code{data.table::fread()}.
#' The data are baseball percent "swing and miss" with their respective average
#' ball velocity and levels of spinrate.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{id}{character giving the observation id}
#'   \item{velocity}{numeric average velocity of the ball}
#'   \item{spinrate}{character factor with 12 levels}
#'   \item{swing_miss}{numeric rate of swing and miss}
#' }
#' @source \href{https://www.fangraphs.com}{Fangraphs}
#'
"spinrates"

#' world_coffee data
#'
#' World coffee production by country (in thousands of 60 kg bags)
#'   is joined with a simple feature of country geometries via their
#'   common parameter `name_long`.
#'
#' @format  Special feature object (class sf) with columns:
#' \describe{
#'   \item{name_long}{character long name of country}
#'   \item{coffee_production_2017}{numeric of coffee production of 60 kg bags}
#'   \item{geom}{the sfc_MULTIPOLYGON country geometries}
#' }
#' @source \code{spData::coffee_data} and \code{spData::world}
#'
"world_coffee"

#' kentucky_counties data
#'
#' A simple feature object with the boundary geometries for nine
#' southeast Kentucky counties.
#' .
#'
#' @format Special feature object (class sf) with columns:
#' \describe{
#'   \item{ID}{character giving the name of the county}
#'   \item{geom}{MULTIPOLYGON of the Kentucky county boundary}
#' }
#' @source \code{maps::map()}
#'
"kentucky_counties"

#' kentucky_elevation data
#'
#' Tiled raster elevation data(in meters) converted to a data frame,
#' covering the southeast region of Kentucky.
#'
#' @format Data frame with columns:
#' \describe{
#'   \item{x}{numeric longitude of the elevation}
#'   \item{y}{numeric latitude of the elevation}
#'   \item{elevation}{the elevation in meters}
#' }
#' @source \code{elevatr::get_elev_raster()} and the
#' Amazon Web Services Terrain Tiles service.
#'
"kentucky_elevation"

#' states data
#'
#' Consolidation of data into one dataframe for the "South" region.
#'
#' @format Data frame for "South" region:
#' \describe{
#'   \item{State}{charactter state name}
#'   \item{Income}{numeric state income}
#'   \item{Murder}{numeric state murder rate}
#' }
#' @source \code{datasets::state_name, state_region, state_x77}
#'
"states"
