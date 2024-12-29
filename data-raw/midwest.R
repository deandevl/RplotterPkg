library(usethis)
library(data.table)
library(ggplot2)

midwest <- data.table::as.data.table(ggplot2::midwest) |>
  _[, .(area, perchsd, percollege, percprof, percwhite, percblack, percasian)] |>
  data.table::setnames(
    old = c("area", "perchsd", "percollege", "percprof", "percwhite", "percblack", "percasian"),
    new = c("Area", "HS_Diploma", "College_Edu", "Prof_Deg", "White", "Black", "Asian")
  )
usethis::use_data(midwest, overwrite = TRUE)
