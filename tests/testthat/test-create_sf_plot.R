test_that("create_sf_plot() sf aes_fill", {
  a_plot <- RplotterPkg::create_sf_plot(
    sf = spData::world,
    title = "World Coffee Production 2017",
    subtitle = "Source: spData::coffee_data",
    panel_color = "white",
    panel_border_color = "white",
  ) |>
  RplotterPkg::create_sf_plot(
    sf = RplotterPkg::world_coffee,
    aes_fill = "coffee_production_2017",
    scale_breaks = seq(0,3000,250),
    scale_labels = seq(0,3000,250),
    scale_limits = c(0,3000),
    scale_colors = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
    legend_key_width = 0.7,
    legend_key_height = 1.0
  )

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("create_sf_plot() sf aes_fill", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
