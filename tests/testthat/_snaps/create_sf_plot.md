# create_sf_plot() sf aes_fill

    Code
      a_plot <- RplotterPkg::create_sf_plot(RplotterPkg::create_sf_plot(sf = spData::world,
      title = "World Coffee Production 2017", subtitle = "Source: spData::coffee_data",
      panel_color = "white", panel_border_color = "white", ), sf = RplotterPkg::world_coffee,
      aes_fill = "coffee_production_2017", scale_breaks = seq(0, 3000, 250),
      scale_labels = seq(0, 3000, 250), scale_limits = c(0, 3000), scale_colors = RColorBrewer::brewer.pal(
        n = 9, name = "YlOrRd"), legend_key_width = 0.7, legend_key_height = 1)
    Message
      Coordinate system already present. Adding new coordinate system, which will replace the existing one.

