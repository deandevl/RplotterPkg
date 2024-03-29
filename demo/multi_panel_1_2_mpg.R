library(data.table)
library(ggplot2)
library(rlang)
library(magrittr)
library(purrr)
library(ggrepel)
library(RplotterPkg)

mpg_dt <- data.table::as.data.table(ggplot2::mpg) %>%
  .[class == "subcompact" | class == "compact"] %>%
  .[, .(class, model, hwy, cty, manufacturer, displ)] %>%
  .[, `:=`(class = as.factor(class), car = paste0(model,"_",displ))]

str(mpg_dt)

classes <- levels(mpg_dt$class)

build_plot <- function(id, dt, classes){
  plot_dt <- dt[class == classes[[id]], ]

  aplot <- RplotterPkg::create_scatter_plot(
    df = plot_dt,
    aes_x = "cty",
    aes_y = "hwy",
    aes_label = "car",
    #position = ggplot2::position_jitter(width = 1.2, height = 1.2),
    pts_size = 3.0,
    pts_fill = "gold",
    x_title = "City MPG",
    y_title = "Highway MPG",
    x_limits = c(10, 35),
    x_major_breaks = seq(from = 10, to = 35, by = 5),
    y_limits = c(20, 45),
    y_major_breaks = seq(from = 20, to = 45, by = 5),
    show_minor_grids = F,
    silent_NA_warning = T
  )
  return(aplot)
}

plot_lst <- purrr::map(
  1:2,
  build_plot,
  dt = mpg_dt,
  classes = classes
)

layout <- list(
  plots = plot_lst,
  rows = c(1, 2),
  cols = c(1, 1)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  title = "City vs Highway MPG Across Compacts",
  plot_titles = c("Subcompact","Compact"),
  cell_width = 24,
  cell_height = 12
)

