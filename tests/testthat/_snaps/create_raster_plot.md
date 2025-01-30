# create_raster_plot() aes_x aes_y aes_fill

    Code
      a_plot <- RplotterPkg::create_raster_plot(df = RplotterPkg::kentucky_elevation,
      title = "County Elevations in Southeast Kentucky", aes_x = "x", aes_y = "y",
      aes_fill = "elevation") + ggplot2::geom_sf(data = RplotterPkg::kentucky_counties,
      aes(x = NULL, y = NULL), alpha = 0, linewidth = 1.5)

