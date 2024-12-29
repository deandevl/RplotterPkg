## code to prepare `religion` dataset goes here
library(usethis)
library(data.table)
library(socviz)

religion <- data.table::as.data.table(socviz::gss_sm) |>
  _[!is.na(religion) & !is.na(happy), .(happy, religion)]

usethis::use_data(religion, overwrite = TRUE)
