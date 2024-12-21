library(ggplot2)
library(rlang)
library(data.table)
library(RplotterPkg)

# The dataset `ggplot2::midwest` has a continuous variable called *area*
#   and a categorical variable called *state*. Show an overlapped density
#   distribution of *area* for 437 counties across 5 states from the midwest
#   dataset by setting class="cd", aes_x = "area", and aes_fill = "state".

midwest_area_dt <- data.table::as.data.table(ggplot2::midwest) |>
  _[, `:=`(state = as.factor(state), area = area * 1000)]

RplotterPkg::create_density_plot(
  df = midwest_area_dt,
  aes_x = "area",
  aes_fill = "state",
  title = "Distribution of area",
  subtitle = "437 counties from midwest dataset",
  x_title = "Area",
  y_title = "Density",
  rot_y_tic_label = TRUE,
  density_color = "black",
  density_alpha = 0.5
)
