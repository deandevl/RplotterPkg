
test_that("multi_panel_grid()", {

  years <- levels(RplotterPkg::gapminder_data$year)

  build_plot <- function(id, dt, years){
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
      title = "Life Expectancy Across Continents",
      x_title = "Life Expectancy",
      y_title = "Population",
      pts_size = 5,
      legend_key_width = 0.8,
      legend_key_height = 0.6,
      show_minor_grids = F,
      silent_NA_warning = TRUE
    )
    return(aplot)
  }

  plot_lst <- purrr::map(1:4,
    build_plot,
    dt = RplotterPkg::gapminder_data,
    years = years
  )

  layout <- list(
    plots = plot_lst,
    # rows = c(1,1,2,2),
    # cols = c(1,2,1,2)
    rows = c(1, 1, 1, 1),
    cols = c(1, 2, 3, 4)
  )

  a_plot <- RplotterPkg::multi_panel_grid(
    layout = layout,
    title = "Life expectancy vs population(millions) across continents",
    plot_titles = c("1952","1972","1992","2002"),
    cell_height = 12
  )

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("multi_panel_grid()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
