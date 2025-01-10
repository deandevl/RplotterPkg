library(usethis)
library(data.table)

state_names <- datasets::state.name
state_region <- as.vector(datasets::state.region)
state_stats <- datasets::state.x77

states <- cbind(state_names, state_region, state_stats) |>
  data.table::as.data.table() |>
  _[state_region == "South", .(State = state_names, Income, Murder)] |>
  _[, `:=`(Income = as.numeric(Income), Murder = as.numeric(Murder))]
usethis::use_data(states, overwrite = TRUE)
