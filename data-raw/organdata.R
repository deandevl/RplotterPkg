library(usethis)
library(data.table)
library(socviz)

organdata <- data.table::as.data.table(socviz::organdata) |>
  _[, country := as.factor(country)] |>
  _[, .(country, donors)] |>
  na.omit()
usethis::use_data(organdata, overwrite = TRUE)
