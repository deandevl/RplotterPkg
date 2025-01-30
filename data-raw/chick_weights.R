library(data.table)
library(usethis)

chick_weights <- data.table::as.data.table(datasets::ChickWeight) |>
  _[Time == 21,
    lapply(.SD, mean),
    by = Diet,
    .SDcols = c("weight")
  ] |>
  _[, Label := c("10%_Pro","20%_Pro","40%_Pro","60%_Pro")] |>
  _[, weight := round(weight, digits = 0)]

usethis::use_data(chick_weights, overwrite = TRUE)
