library(ggplot2)
library(data.table)
library(magrittr)
library(ggrepel)
library(socviz)
library(RplotterPkg)
library(here)

str(socviz::organdata)

organdata_dt <- data.table::as.data.table(socviz::organdata) %>%
  .[, country := as.factor(country)]

donor_plot <- RplotterPkg::create_box_plot(
  df = organdata_dt,
  aes_x = "country",
  aes_y = "donors",
  aes_label = "donors",
  aes_label_color = "red",
  order_by_median = "desc",
  y_limits = c(5,35),
  y_major_breaks = seq(5, 35, 5),
  title = "Organ Donation Rate per Million",
  subtitle = "Showing outlier rates (source: socviz::organdata)",
  x_title = "Country",
  y_title = "Donor Rate",
  box_color = "purple",
  box_size = 0.8,
  rot_x_tic_angle = 40,
  rot_y_tic_label = TRUE,
  ol_color = "red",
  ol_size = 1.5,
  silent_NA_warning = T
 # png_file_path = file.path(here::here(),"plots.png")
)
donor_plot
