# create_box_plot() aes_y aes_label

    Code
      set.seed(224)
      air_df <- na.omit(datasets::airquality)
      a_plot <- RplotterPkg::create_box_plot(df = air_df, aes_y = "Ozone", aes_label = "Month",
        ol_color = "red", ol_size = 1.5, title = "Air Quality", subtitle = "Showing outlier months July(7), August(8)",
        y_title = "Ozone (ppb)", box_fill = "brown", box_alpha = 0.7, y_limits = c(0,
          200), y_major_breaks = seq(from = 0, to = 200, by = 20), rot_y_tic_label = TRUE)

# create_box_plot() aes_x, aes_y aes_label

    Code
      set.seed(224)
      a_plot <- RplotterPkg::create_box_plot(df = RplotterPkg::organdata, aes_x = "country",
      aes_y = "donors", aes_label = "donors", order_by_median = "desc", y_limits = c(
        5, 35), y_major_breaks = seq(5, 35, 5), title = "Organ Donation Rate per Million",
      subtitle = "Showing outlier rates", center_titles = TRUE, x_title = "Country",
      y_title = "Donor Rate", do_coord_flip = TRUE, box_color = "purple",
      box_line_width = 0.8, rot_y_tic_label = TRUE, ol_color = "red", ol_size = 1.5)

# create_box_plot() aes_y, aes_fill aes_color

    Code
      set.seed(224)
      orange_df <- data.table::as.data.table(datasets::Orange)[, Tree := factor(Tree,
        levels = c("1", "2", "3", "4", "5"))]
      a_plot <- RplotterPkg::create_box_plot(df = orange_df, aes_y = "circumference",
        aes_fill = "Tree", aes_color = "Tree", title = "Growth of Orange Trees",
        subtitle = "source: datasets::Orange")

