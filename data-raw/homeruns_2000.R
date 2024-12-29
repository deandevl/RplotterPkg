## code to prepare `homeruns_2000` dataset goes here
library(usethis)
library(data.table)
library(here)

data_file_path <- file.path(here::here(), "data-raw", "homeruns_2000.txt")
homeruns_2000 <- data.table::fread(data_file_path) |>
  _[, YEARS := as.factor(YEARS)]
usethis::use_data(homeruns_2000, overwrite = TRUE)
