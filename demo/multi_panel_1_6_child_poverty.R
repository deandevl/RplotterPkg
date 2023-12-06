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
  aplot <- RplotterPkg::create_scatter_plot(
    df = dt,
    aes_x = variables[[id]],
    aes_y = "Child_Poverty",
    y_title = "% Child Poverty",
    show_minor_grids = F,
    silent_NA_warning = T
  )
  return(aplot)
}

plot_lst <- purrr::map(1:6,
  build_plot,
  dt = child_poverty_dt,
  variables = variables
)

layout <- list(
  plots = plot_lst,
  rows = c(1, 1, 1, 1, 1, 1),
  cols = c(1, 2, 3, 4, 5, 6)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  title = "Midwest Child Poverty Across Percent Population Segments",
  plot_titles = c("HS_Diploma", "College", "Professioinal", "White", "Black", "Asian"),
  cell_width = 6,
  cell_height = 12
)

