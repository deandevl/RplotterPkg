library(data.table)
library(ggplot2)
library(rlang)
library(magrittr)
library(datasets)
library(purrr)
library(RplotterPkg)

trees_z_dt <- data.table::as.data.table(datasets::trees) %>%
  .[ ,`:=`(
      Girth_z = (Girth - mean(Girth))/sd(Girth),
      Height_z = (Height - mean(Height))/sd(Height),
      Volume_z = (Volume - mean(Volume))/sd(Volume)
    )]

trees_z_long_dt <- data.table::melt(
  trees_z_dt,
  measure.vars = c("Girth_z", "Height_z", "Volume_z"),
  value.name = "Measure"
)

str(trees_z_long_dt)

measures <- c("Girth_z", "Height_z", "Volume_z")

build_plot <- function(id, dt, measures){
  plot_dt <- dt[variable == measures[[id]], ]

  aplot <- RplotterPkg::create_histogram_plot(
    df = plot_dt,
    aes_x = "Measure",
    y_title = "Count",
    bar_color = "black",
    bar_fill = "green",
    bar_labels = T,
    bar_label_size = 3,
    binwidth = 0.5,
    x_limits = c(-3.0,4.0),
    x_major_breaks = seq(from = -3.0, to = 4.0, by = 1.0),
    y_limits = c(0,8)
  )
  return(aplot)
}

plot_lst <- purrr::map(1:3,
  build_plot,
  dt = trees_z_long_dt,
  measures = measures
)

layout <- list(
  plots = plot_lst,
  rows = c(1, 1, 1),
  cols = c(1, 2, 3)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  title = "Girth, Height, Volume of data(trees)",
  plot_titles = c("Girth_z", "Height_z", "Volume_z"),
  cell_width = 12,
  cell_height = 16
)
