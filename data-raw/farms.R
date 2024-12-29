library(usethis)
library(data.table)
library(here)

data_file_path <- file.path(here::here(), "data-raw", "farms.txt")
farms <- data.table::fread(data_file_path)
usethis::use_data(farms, overwrite = TRUE)
