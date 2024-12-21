library(data.table)
library(ggplot2)
library(purrr)
library(RplotterPkg)

# Create a data frame that focuses on the percent of child poverty
#  across 5 midwest states and their counties

child_poverty_dt <- data.table::as.data.table(ggplot2::midwest) |>
  _[, .(state, county, perchsd, percchildbelowpovert)] |>
  _[, state := as.factor(state)] |>
  data.table::setorderv(cols = "percchildbelowpovert", order = -1) |>
  data.table::setnames(old = c("state", "perchsd", "percchildbelowpovert"),
                       new = c("State", "High_School", "Child_Poverty"))

str(child_poverty_dt)


# Create 5 scatter plots that shows percent of high school diplomas versus
#   percent of child poverty across 5 states
states <- levels(child_poverty_dt$State)

# Define a function for creating a plot
build_plot <- function(id, dt, states){
  state <- states[[id]]
  plot_dt <- dt[State == state,]

  aplot <- RplotterPkg::create_scatter_plot(
    df = plot_dt,
    aes_x = "High_School",
    aes_y = "Child_Poverty",
    x_title = "% HS Diploma",
    y_title = "% Child Poverty",
    subtitle = state,
    x_limits = c(40,90),
    x_major_breaks = seq(40,90,10),
    y_limits = c(0,60),
    y_major_breaks = seq(0,60,10),
    show_minor_grids = FALSE,
    silent_NA_warning = TRUE
  )
  return(aplot)
}

plot_lst <- purrr::map(1:5,
  build_plot,
  dt = child_poverty_dt,
  states = states
)

# Summarize the child poverty across the states by creating a graphic summary table
mean_child_poverty_dt <- child_poverty_dt[, .(N = .N,  Mean_Child_Poverty = unlist(lapply(.SD, mean))), by = State, .SDcols = "Child_Poverty"] |>
_[, Mean_Child_Poverty := round(Mean_Child_Poverty, 2)] |>
data.table::setorderv(cols = c("Mean_Child_Poverty"), order = -1)

child_poverty_table <- RplotterPkg::create_table_graphic(
  df = mean_child_poverty_dt,
  table_width = 8,
  title = "Mean Percent of Child Poverty Across Midwest Counties",
  show_row_names = FALSE,
  display_plot = FALSE
)

# Create a text caption grob
caption_text_grob <- grid::textGrob(label = "Adult Education and Child Poverty", gp = grid::gpar(col = "black", fontsize = 12, fontface = 2))

# Create a layout that describes the placement of the above plots, table, and caption
layout <- list(
  plots = list(
    child_poverty_table,
    plot_lst[[1]],
    plot_lst[[2]],
    plot_lst[[3]],
    plot_lst[[4]],
    plot_lst[[5]],
    caption_text_grob), # plot/grob objects to display
  rows = c(1, 2, 2, 2, 2, 2, 3),
  cols = c(3, 1, 2, 3, 4, 5, 3)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  do_grid = TRUE,
  cell_width = 8
)
