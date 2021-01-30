library(data.table)
library(palmerpenguins)
library(ggplot2)
library(RplotterPkg)

# -------------First demo on ggplot::economics data set-----------------
RplotterPkg::create_scatter_plot(
  df = ggplot2::economics,
  aes_x = "date",
  aes_y = "unemploy",
  x_title = "Date",
  y_title = "Unemployment",
  rot_y_tic_label = TRUE,
  title = "US Monthly Unemployment",
  subtitle = "1967 to 2014 (in thousands)",
  pts_color = "blue",
  line_color = "violet",
  connect = TRUE,
  x_date_labels = "%Y",
  x_major_breaks = seq.Date(from = as.Date("1964-01-01"), to = as.Date("2020-01-01"), by = "4 year"),
  show_minor_grids = FALSE
) + geom_hline(aes(yintercept = 8000), color = "red", linetype = "dashed", lwd = 1)

# -----------palmerpenguins: mean flipper length over years by species----------
data("penguins", package = "palmerpenguins")

penguins_dt <- data.table::setDT(penguins)
penguins_dt <- na.omit(penguins_dt)
penguins_mean_flipper_dt <- penguins_dt[,
                                        lapply(.SD, mean),
                                        by = .(year, species),
                                        .SDcols = "flipper_length_mm"]
data.table::setnames(penguins_mean_flipper_dt,
                     old = c("year", "species", "flipper_length_mm"),
                     new = c("Year", "Species", "Mean Flipper Length"))
print(penguins_mean_flipper_dt)

RplotterPkg::create_scatter_plot(
  df = penguins_mean_flipper_dt,
  aes_x = "Year",
  aes_y = "Mean Flipper Length",
  aes_linetype = "Species",
  title = "Mean Flipper Length by Species Across the Years",
  x_major_breaks = c(2007,2008,2009),
  show_minor_grids = F,
  palette_linetypes = c("dashed", "twodash", "dotdash")
)
