library(data.table)

sleep <- data.table::as.data.table(datasets::sleep) |>
  _[, Observation := seq(1,20)] |>
  data.table::setnames(old = c("extra","group"), new = c("Hours","Drug"))

usethis::use_data(sleep, overwrite = TRUE)
