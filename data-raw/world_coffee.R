library(data.table)
library(spData)
library(usethis)

# convert the coffee data as a data.table
coffee_dt <- data.table::as.data.table(spData::coffee_data)

# convert the spData::world as a data.table
world_dt <- data.table::as.data.table(spData::world)

# join the two data.tables based on common "name_long" column
coffee_world_joined_dt <- coffee_dt[world_dt, on = c("name_long", "name_long")]

# select columns, remove NA, and convert joined data.table to sf class
world_coffee <- coffee_world_joined_dt[, .(name_long, coffee_production_2017, geom)] |>
  na.omit() |>
  sf::st_as_sf()

usethis::use_data(world_coffee, overwrite = TRUE)
