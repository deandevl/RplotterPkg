library(usethis)
library(data.table)
library(here)

data_file_path <- file.path(here::here(), "data-raw", "spinrates.csv")
spinrates <- data.table::fread(data_file_path)
usethis::use_data(spinrates, overwrite = TRUE)
