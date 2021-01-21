# Objective : Demos RplotterPkg::multi_scatter_plot()
# Created by: Rick Dean
# Created on: 2020-05-31 8:38 AM

library(data.table)
library(rlang)
library(grid)
library(gtable)
library(ggplot2)
library(dplyr)
library(RplotterPkg)

# from ggplot2::midwest, set a number of x axis variables vs "percchildbelowpovert" in a set of scatter plots
RplotterPkg::multi_scatter_plot(
  df = ggplot2::midwest,
  variables = c("perchsd", "percollege", "percprof", "percwhite", "percblack", "percasian"),
  aes_y = "percchildbelowpovert",
  columns = 3,
  title = "Child Poverty Across Population Segments"
)

# from gapminder::gapminder data set plot "lifeExp" vs "pop" across levels
#  of "year" and "continent"
str(gapminder::gapminder)

gapminder_df <- gapminder::gapminder %>%
  filter(year > 1987 & continent %in% c("Africa", "Americas", "Asia", "Europe")) %>%
  droplevels() %>%
  mutate(
    year = factor(year),
    pop = pop/1e+6
  )
dplyr::glimpse(gapminder_df)

RplotterPkg::multi_scatter_plot(
  df = gapminder_df,
  factor_var = "year",
  factor_x = "lifeExp",
  aes_y = "pop",
  aes_fill = "continent",
  x_limits = c(20,90),
  x_major_breaks = seq(from = 20, to = 90, by = 10),
  y_limits = c(0,300),
  y_major_breaks = seq(from = 0, to = 300, by = 50),
  rot_y_tic_label = TRUE,
  title = "Life expectancy vs population(millions) across continents",
  subtitle = "dataset: gapminder",
  pts_size = 3.0,
  pts_line_alpha = 0.6,
  axis_text_size = 9,
  silent_NA_warning = TRUE
)

# from gapminder::gapminder data set plot "Year" vs "GDP" across levels
#  of "Country" and "Life_Exp"
gapminder_country_df <- gapminder::gapminder %>%
  filter(year > 1987 & country %in% c("Afghanistan", "Pakistan", "China", "Japan", "Korea, Dem. Rep.", "Vietnam")) %>%
  rename(Year = year, Country = country, Life_Exp = lifeExp, GDP = gdpPercap) %>%
  select(Year, Country, Life_Exp, GDP)
dplyr::glimpse(gapminder_country_df)

RplotterPkg::multi_scatter_plot(
  df = gapminder_country_df,
  factor_var = "Country",
  factor_x = "Year",
  columns = 3,
  aes_y = "GDP",
  aes_fill = "Life_Exp",
  title = "Life Expect Changes for Year vs GDP across Countries",
  pts_size = 4.0,
  line_size = 0.6,
  connect = TRUE
)