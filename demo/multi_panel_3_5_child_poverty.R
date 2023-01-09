library(data.table)
library(ggplot2)
library(magrittr)
library(purrr)
library(RplotterPkg)

# Create a data frame that focuses on the percent of child poverty
#  across 5 midwest states and their counties

child_poverty_dt <- data.table::as.data.table(ggplot2::midwest) %>%
  .[, .(state, county, perchsd, percchildbelowpovert)] %>%
  .[, state := as.factor(state)] %>%
  data.table::setorderv(., cols = "percchildbelowpovert", order = -1) %>%
  data.table::setnames(., old = c("state", "perchsd", "percchildbelowpovert"),
                       new = c("State", "High_School", "Child_Poverty"))

str(child_poverty_dt)


# Create 5 scatter plots that shows percent of high school diplomas versus
#   percent of child poverty across 5 states
states <- levels(child_poverty_dt$State)

# Define a function for creating a plot
build_plot <- function(id, dt, states){
  y_title <- NULL
  hide_y_tics <- FALSE
  if(id == 1){
    y_title  <- "% Child Poverty"
  }else {
    hide_y_tics <- TRUE
  }
  state <- states[[id]]
  plot_dt <- dt[State == state,]

  aplot <- RplotterPkg::create_scatter_plot(
    df = plot_dt,
    aes_x = "High_School",
    aes_y = "Child_Poverty",
    subtitle = state,
    x_title = "% HS Diploma",
    y_title = y_title,
    hide_y_tics = hide_y_tics,
    x_limits = c(40,90),
    x_major_breaks = seq(40,90,10),
    y_limits = c(0,60),
    y_major_breaks = seq(0,60,10),
    show_minor_grids = F,
    silent_NA_warning = T
  )
  return(aplot)
}

plot_lst <- purrr::map(1:5,
                        build_plot,
                        dt = child_poverty_dt,
                        states = states
)

# Summarize the child poverty across the states by creating a graphic summary table
mean_child_poverty_dt <- child_poverty_dt[, .(N = .N,  Mean_Child_Poverty = unlist(lapply(.SD, mean))), by = State, .SDcols = "Child_Poverty"] %>%
.[, Mean_Child_Poverty := round(Mean_Child_Poverty, 2)] %>%
data.table::setorderv(., cols = c("Mean_Child_Poverty"), order = -1)

child_poverty_table <- RplotterPkg::create_table_graphic(
  df = mean_child_poverty_dt,
  table_width = 8,
  title = "Mean Percent of Child Poverty Across Midwest Counties",
  show_row_names = FALSE,
  display_plot = F
)

# Create a text caption grob
caption_text_grob <- grid::textGrob(label = "There appears to be a relationship between adult education and child poverty", gp = grid::gpar(col = "black", fontsize = 14, fontface = 2))

# Create a layout that describes the placement of the above plots, table, and caption
layout <- list(
  plots = list(child_poverty_table, plot_lst[["IL"]], plot_lst[["IN"]], plot_lst[["MI"]], plot_lst[["OH"]], plot_lst[["WI"]], caption_text_grob), # plot/grob objects to display
  rows = c(1, 2, 2, 2, 2, 2, 3),
  cols = c(list(1:5), 1, 2, 3, 4, 5, list(1:5))
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(6.4, 5.8, 5.8, 5.8, 5.8),
  row_heights = c(7.5, 7.4, 2),
  title = "Child Poverty vs Adult Education in counties across midwest states"
)
