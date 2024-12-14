library(ggplot2)
library(data.table)
library(RplotterPkg)

people_weights_df <- data.frame(
  name = as.factor(c("John", "Alice", "Peter", "Paul", "Tom")),
  weight = c(190, 110, 210, 147, 165)
)

weights_plot <- RplotterPkg::create_bar_plot(
  df = people_weights_df,
  aes_x = "name",
  aes_y = "weight",
  x_title = "Name",
  y_title = "Weight",
  bar_fill = "red",
  bar_labels = TRUE,
  order_bars = "desc",
  rot_y_tic_label = TRUE
)

weights_plot
