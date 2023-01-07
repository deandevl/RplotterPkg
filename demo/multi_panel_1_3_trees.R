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
  y_title <- NULL
  hide_y_tics <- FALSE
  if(id == 1){
    y_title <- "Count"
  }else{
    hide_y_tics <- TRUE
  }

  plot_dt <- dt[variable == measures[[id]], ]

  aplot <- RplotterPkg::create_histogram_plot(
    df = plot_dt,
    aes_x = "Measure",
    title = measures[[id]],
    y_title = y_title,
    hide_y_tics = hide_y_tics,
    bar_color = "black",
    bar_fill = "green",
    bar_labels = T,
    binwidth = 0.5,
    x_limits = c(-3,3.5),
    x_major_breaks = seq(from = -3, to = 3.5, by = 0.5),
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
  col_widths =  c(5.4, 5, 5),
  row_heights = 5,
  title = "Girth, Height, Volume of data(trees)"
)
