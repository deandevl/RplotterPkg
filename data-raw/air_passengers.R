library(usethis)
library(tsbox)

air_passengers <- tsbox::ts_df(datasets::AirPassengers)
usethis::use_data(air_passengers, overwrite = TRUE)
