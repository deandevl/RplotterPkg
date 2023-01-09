library(data.table)
library(ggplot2)
library(here)
library(RregressPkg)
library(RplotterPkg)

# This demo has 2 ggplot2 objects with layout of 1 row 14 centimeters high and
# 2 columns 15.4 and 15 centimeters wide.

data_path <- file.path(here::here(), "demo", "data", "alcoholarm.txt")

# Set the data:
alchoholarm_dt <- data.table::fread(data_path)

# Estimate the reduced and full models:
reduced_ols <- mean(alchoholarm_dt$strength)
reduced_ss <- sum((alchoholarm_dt$strength - reduced_ols)^2)

full_ols <- RregressPkg::ols_calc(
  df = alchoholarm_dt,
  formula_obj = strength ~ alcohol
)
full_ss <- full_ols$sse

# Create the plot data frame and display the reduced and full models:
plot_df <- data.frame(
  Alcohol = alchoholarm_dt$alcohol,
  Strength = alchoholarm_dt$strength,
  Strength_R = reduced_ols,
  Strength_F = full_ols$fitted_val
)

reduced_plot <- RplotterPkg::create_scatter_plot(
  df = plot_df,
  aes_x = "Alcohol",
  aes_y = "Strength",
  subtitle = paste0("Reduced Model: strength ~ mean(strength); SSE = ",round(reduced_ss,digits = 2)),
  x_title = "Alcohol",
  y_title = "Strength",
  rot_y_tic_label = T,
  bold_y = reduced_ols,
  bold_y_color = "red"
)

full_plot <- RplotterPkg::create_scatter_plot(
  df = plot_df,
  aes_x = "Alcohol",
  aes_y = "Strength",
  subtitle = paste0("Full Model: strength ~ alcohol; SSE = ",round(full_ss, digits = 2)),
  rot_y_tic_label = T,
  x_title = "Alcohol",
  y_title = NULL,
  hide_y_tics = T
) + geom_line(aes(x = Alcohol, y = Strength_F), color="red")

layout <- list(
  plots = list(reduced_plot, full_plot),
  rows = c(1,1),
  cols = c(1,2)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(15.4,15),
  row_heights = 14,
  title = "Reduced and Full Model of Alcohol vs Strength"
)

