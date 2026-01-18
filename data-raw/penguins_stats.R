## code to prepare `penguins_stats` dataset goes here
library(usethis)
library(data.table)
library(palmerpenguins)

data(penguins, package = "palmerpenguins")

penguins_dt <- data.table::as.data.table(penguins) |>
  na.omit()

penguins_stats <- penguins_dt[, .(
  avg_body_mass = mean(body_mass_g),
  min_body_mass = min(body_mass_g),
  max_body_mass = max(body_mass_g)
), by = species] |>
_[, mid_labels := as.character(round(df$avg_body_mass))]

usethis::use_data(penguins_stats, overwrite = TRUE)
