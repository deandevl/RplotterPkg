## code to prepare `gapminder_data` dataset goes here

library(gapminder)
library(data.table)
library(usethis)

gapminder_data <- data.table::as.data.table(gapminder::gapminder) |>
  _[year %in% c(1952, 1972, 1992, 2002) & continent %in% c("Africa", "Americas", "Asia", "Europe")] |>
  droplevels() |>
  _[, `:=`(year = as.factor(year), pop = pop/1e+6)]

usethis::use_data(gapminder_data, overwrite = TRUE)
