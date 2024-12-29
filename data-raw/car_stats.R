library(usethis)
library(data.table)

car_stats <- data.table::as.data.table(datasets::mtcars) |>
_[, .(
   N = .N,
   mean_mpg = round(mean(mpg),2),
   mean_hp = round(mean(hp),2),
   mean_wt = round(mean(wt),2),
   mean_disp = round(mean(disp),2)
  ), by = cyl
]

usethis::use_data(car_stats, overwrite = TRUE)
