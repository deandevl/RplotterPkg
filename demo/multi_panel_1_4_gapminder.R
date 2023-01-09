library(ggplot2)
library(rlang)
library(magrittr)
library(purrr)
library(gapminder)
library(RplotterPkg)

gapminder_dt <- data.table::as.data.table(gapminder::gapminder) %>%
  .[year %in% c(1952, 1972, 1992, 2002) & continent %in% c("Africa", "Americas", "Asia", "Europe")] %>%
  droplevels(.) %>%
  .[, `:=`(year = as.factor(year), pop = pop/1e+6)]

str(gapminder_dt)

years <- levels(gapminder_dt$year)

build_plot <- function(id, dt, years){
  y_title <- NULL
  hide_y_tics <- FALSE
  if(id == 1){
    y_title  <- "Population"
  }else {
    hide_y_tics <- TRUE
  }

  show_legend <- F
  if(id == 4){
    show_legend <- T
  }

  plot_dt <- dt[year == years[[id]], ]

  aplot <- RplotterPkg::create_scatter_plot(
    df = plot_dt,
    aes_x = "lifeExp",
    aes_y = "pop",
    aes_fill = "continent",
    x_limits = c(20, 80),
    x_major_breaks = seq(from = 20, to = 80, by = 10),
    y_limits = c(0, 400),
    y_major_breaks = seq(from = 0, to = 400, by = 50),
    subtitle = years[[id]],
    x_title = "Life Expectancy",
    y_title = y_title,
    hide_y_tics = hide_y_tics,
    pts_size = 5,
    show_legend = show_legend,
    legend_key_width = 0.8,
    legend_key_height = 0.6,
    show_minor_grids = F,
    silent_NA_warning = T
  )
  return(aplot)
}

plot_lst <- purrr::map(1:4,
                       build_plot,
                       dt = gapminder_dt,
                       years = years
)

layout <- list(
  plots = plot_lst,
  rows = c(1, 1, 1, 1),
  cols = c(1, 2, 3, 4)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(8.2, 7.8, 7.8, 10.6),
  row_heights = 10,
  title = "Life expectancy vs population(millions) across continents"
)
