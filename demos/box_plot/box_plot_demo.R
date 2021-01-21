library(data.table)
library(rlang)
library(ggplot2)
library(ggrepel)
library(socviz)
library(RplotterPkg)

str(socviz::organdata)

data("organdata")
str(organdata)

# unordered box plot; outliers are labeled by row number
RplotterPkg::create_box_plot(
  df = organdata,
  aes_x = "country",
  aes_y = "donors",
  y_limits = c(5,35),
  y_major_breaks = seq(5, 35, 5),
  title = "Organ Donation Rate per Million",
  subtitle = "Showing row numbers of outliers",
  center_titles = TRUE,
  x_title = "Country",
  y_title = "Donor Rate",
  box_color = "purple",
  box_size = 0.8,
  rot_x_tic_angle = 40,
  rot_y_tic_label = TRUE,
  ol_color = "red",
  ol_size = 1.5,
  show_outliers = TRUE
)

# ordering aes_x variable in descending order based on median of aes_y variable
# outliers labeled with "donors" values
RplotterPkg::create_box_plot(
  df = organdata,
  aes_x = "country",
  aes_y = "donors",
  show_outliers = TRUE,
  label_outlier_var = "donors",
  label_outlier_color = "blue",
  y_limits = c(5,35),
  y_major_breaks = seq(5, 35, 5),
  title = "Organ Donation Rate per Million",
  subtitle = "Showing 'donors' value for outliers",
  center_titles = TRUE,
  x_title = "Country",
  y_title = "Donor Rate",
  box_color = "purple",
  box_size = 0.8,
  rot_x_tic_angle = 40,
  rot_y_tic_label = TRUE,
  ol_color = "red",
  ol_size = 1.5,
  order_by_median = "desc"
)




