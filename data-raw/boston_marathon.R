library(usethis)
library(data.table)
library(here)

data_file_path <- file.path(here::here(), "data-raw", "boston_marathon.txt")
boston_marathon <- data.table::fread(data_file_path)
usethis::use_data(boston_marathon, overwrite = TRUE)
