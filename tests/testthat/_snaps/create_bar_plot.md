# create_bar_plot() aes_x aes_fill

    Code
      a_plot <- RplotterPkg::create_bar_plot(df = RplotterPkg::religion, aes_x = "happy",
      aes_fill = "religion", position = "dodge", title = "Happy Religions",
      center_titles = TRUE, rot_y_tic_label = TRUE, bar_width = 0.8, order_bars = "desc",
      x_title = "Happiness", y_title = "Count", axis_text_size = 16)

# create_bar_plot() aes_x aes_y aes_fill aes_color

    Code
      a_plot <- RplotterPkg::create_bar_plot(df = RplotterPkg::chick_weights, aes_x = "Label",
      aes_y = "weight", aes_fill = "Diet", aes_color = "Diet", bar_labels = TRUE,
      x_title = "Chick Diet", y_title = "Chick Weight", order_bars = "asc")

# create_bar_plot() major_breaks aes_x

    Code
      a_plot <- RplotterPkg::create_bar_plot(df = datasets::ToothGrowth, aes_x = "len",
      x_major_breaks = seq(from = 0, to = 40, by = 5), y_major_breaks = seq(from = 0,
        to = 16, by = 2), y_limits = c(0, 16), bar_labels = TRUE, bar_fill = "blue",
      bar_alpha = 0.6, title = "Tooth Growth", subtitle = "source: datasets::ToothGrowth",
      x_title = "Length", y_title = "Count")

