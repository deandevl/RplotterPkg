# create_heatmap() aes_x aes_y aes_fill aes_label

    Code
      a_plot <- RplotterPkg::create_heatmap(df = RplotterPkg::spinrates, aes_x = "velocity",
      aes_y = "spinrate", aes_fill = "swing_miss", aes_label = "swing_miss",
      label_fontface = "bold", title = "Likelihood of swinging and missing on a fastball",
      x_title = "Velocity", y_title = "Spinrate", label_sz = 5, rot_y_tic_label = TRUE) +
        ggplot2::scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
        n.breaks = 8) + ggplot2::guides(fill = ggplot2::guide_colorbar(ticks.colour = "black"))

