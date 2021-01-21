library(data.table)
library(rlang)
library(grid)
library(gtable)
library(ggplot2)
library(socviz)
library(RplotterPkg)

white_black_df <- socviz::gss_sm %>%
  filter(race %in% c("White", "Black"))
age_race_plot <- RplotterPkg::multi_histogram_plot(
  df = white_black_df,
  factor_var = "marital",
  factor_x = "age",
  title = "Distribution of age across marital status and race",
  subtitle = "dataset: socviz::gss_sm",
  rot_y_tic_label = TRUE,
  aes_fill = "race",
  position = "identity",
  x_limits = c(20,90),
  x_major_breaks = seq(20,90,10),
  palette_colors = c("white", "black"),
  bar_alpha = 0.5,
  binwidth = 5,
  row_height = 3.5,
  silent_NA_warning = TRUE
)

