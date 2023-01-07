library(ggplot2)
library(rlang)
library(magrittr)
library(purrr)
library(RplotterPkg)

child_poverty_dt <- data.table::as.data.table(ggplot2::midwest) %>%
    data.table::setnames(.,
      old = c("percchildbelowpovert", "perchsd", "percollege", "percprof", "percwhite", "percblack", "percasian"),
      new = c("Child_Poverty", "HS_Diploma", "College", "Professioinal", "White", "Black", "Asian")) %>%
    .[, .(Child_Poverty, HS_Diploma, College, Professioinal, White, Black, Asian)]


variables <- c("HS_Diploma", "College", "Professioinal", "White", "Black", "Asian")

build_plot <- function(id, dt, variables){
  y_title <- NULL
  hide_y_tics <- FALSE
  if(id == 1){
    y_title <- "% Child Poverty"
  }else {
    hide_y_tics <- TRUE
  }

  aplot <- RplotterPkg::create_scatter_plot(
    df = dt,
    aes_x = variables[[id]],
    aes_y = "Child_Poverty",
    title = variables[[id]],
    y_title = y_title,
    hide_y_tics = hide_y_tics,
    show_minor_grids = F,
    silent_NA_warning = T
  )
  return(aplot)
}

plot_lst <- purrr::map(1:6, build_plot, dt = child_poverty_dt, variables = variables)

layout <- list(
  plots = plot_lst,
  rows = c(1, 1, 1, 1, 1, 1),
  cols = c(1, 2, 3, 4, 5, 6)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(3.0, 2.5, 2.5, 2.5, 2.5, 2.5),
  row_heights = 3,
  title = "Midwest Child Poverty Across Percent Population Segments",
  subtitle = "Source: ggplot2::midwest"
)

