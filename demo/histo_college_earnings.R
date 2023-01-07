library(ggplot2)
library(data.table)
library(rlang)
library(magrittr)
library(causaldata)
library(RplotterPkg)

data("scorecard",package = "causaldata")
ratiovec <- function(n,top){
  rate <- top^(1/(n-1))
  v <- c(1)
  for(i in 2:n){
    v[i] <- v[i-1]*rate
  }
  return(v)
}

scorecard_dt <- data.table::as.data.table(scorecard) %>%
  .[, .(earnings_med)] %>%
  na.omit(.)

RplotterPkg::create_histogram_plot(
  df = scorecard_dt,
  aes_x = "earnings_med",
  title = "Median Earnings of College Graduates",
  subtitle = "Source: scorecard 2007",
  x_title = "Median Earnings (Log Scale)",
  y_title = "Count",
  bins = 5,
  x_log10 = TRUE,
  x_major_breaks = ratiovec(6,32) * 10000,
  x_labels = scales::label_dollar(),
  y_limits = c(0,20000),
  y_major_breaks = seq(0,20000,5000),
  rot_y_tic_label = TRUE,
  show_minor_grids = FALSE,
  show_major_grids = FALSE
)
