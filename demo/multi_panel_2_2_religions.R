library(data.table)
library(socviz)
library(ggplot2)
library(rlang)
library(magrittr)
library(purrr)
library(RplotterPkg)

percent_religion_dt <- data.table::as.data.table(socviz::gss_sm) %>%
  .[!is.na(religion) & !is.na(happy)] %>%
  .[, list(N = .N),   by = list(bigregion, religion)] %>%
  .[, list(N = N, Religion = religion,  Total = sum(N), Percent = N/sum(N) * 100), by = bigregion]

str(percent_religion_dt)

regions <- levels(percent_religion_dt$bigregion)

build_plot <- function(id, dt, regions){
  hide_y_tics <- FALSE
  if(id == 1 | id == 3){
    y_title <-  "Percent"
  }else {
    y_title <- NULL
    hide_y_tics <- TRUE
  }

  plot_dt <- dt[bigregion == regions[[id]],]

  aplot <- RplotterPkg::create_bar_plot(
    df = plot_dt,
    aes_x = "Religion",
    aes_y = "Percent",
    subtitle = regions[[id]],
    x_title = "Religion",
    y_title = y_title,
    hide_y_tics = hide_y_tics,
    rot_y_tic_label = T,
    bar_fill = "lightblue",
    y_limits = c(0.0, 70.0),
    y_major_breaks = seq(from = 0, to = 70, by = 10)
  )

  return(aplot)
}

plot_lst <- purrr::map(1:4,
                       build_plot,
                       dt = percent_religion_dt,
                       regions = regions
                       )

layout <- list(
  plots = plot_lst,
  rows = c(1, 1, 2, 2),
  cols = c(1, 2, 1, 2)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(10.4,10),
  row_heights = c(9,9),
  title = "Percent of religions across US regions"
)


