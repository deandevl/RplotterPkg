library(data.table)
library(palmerpenguins)
library(ggplot2)
library(RplotterPkg)

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
                     new = c("Year", "Species", "Mean_Flipper_Length"))
str(penguins_mean_flipper_dt)

RplotterPkg::create_scatter_plot(
  df = penguins_mean_flipper_dt,
  aes_x = "Year",
  aes_y = "Mean_Flipper_Length",
  x_title = "Year",
  y_title = "Mean Flipper Length",
  aes_linetype = "Species",
  title = "Mean Flipper Length by Species Across the Years",
  x_major_breaks = c(2007,2008,2009),
  show_minor_grids = FALSE,
  panel_border_color = "green",
  connect = TRUE
) +
ggplot2::guides(
  linetype = guide_legend(
    override.aes = list(size = 1.5)
  )
)
