library(data.table)
library(rlang)
library(ggplot2)
library(RColorBrewer)
library(here)
library(RplotterPkg)

file_path <- file.path(here::here(), "demo", "data", "spinrates.csv")

spinrates_dt <- data.table::fread(file = file_path)

RplotterPkg::create_heatmap(
  df = spinrates_dt,
  aes_x = "velocity",
  aes_y = "spinrate",
  aes_fill = "swing_miss",
  aes_label = "swing_miss",
  label_fontface = "bold",
  title = "Likelihood of swinging and missing on a fastball",
  x_title = "Velocity",
  y_title = "Spinrate",
  rot_y_tic_label = T
) +
ggplot2::scale_fill_gradientn(
  colors = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
  n.breaks = 8
) +
ggplot2::guides(
  fill = ggplot2::guide_colorbar(
    ticks.colour = "black"
  )
)

