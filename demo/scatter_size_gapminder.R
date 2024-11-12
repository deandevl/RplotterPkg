library(ggplot2)
library(RColorBrewer)
library(rlang)
library(data.table)
library(magrittr)
library(gapminder)
library(RplotterPkg)

# Get gapminder data for year 2007 as a data.table
# Set  with value of "Oceania" as "Asia"
gapminder_dt <- data.table::as.data.table(gapminder::gapminder)
gapminder_dt$continent <- as.character(gapminder_dt$continent)
gapminder_2007_dt <- gapminder_dt[year == 2007,] %>%
  .[, continent_2 := ifelse(continent == "Oceania", "Asia", continent)] %>%
  .[, continent_2 := as.factor(continent_2)]

str(gapminder_2007_dt)

palette_func <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 4, name = "Set1"))

RplotterPkg::create_scatter_plot(
  df = gapminder_2007_dt,
  aes_x = "gdpPercap",
  aes_y = "lifeExp",
  title = "Income versus Life Expectancy",
  x_title = "Income",
  y_title = "Life Expectancy",
  aes_fill = "continent_2",
  aes_size = "pop",
  rot_y_tic_label = T,
  x_log10 = T,
  show_legend = T,
  x_limits = c(250,64000),
  x_major_breaks = c(250, 500, 1000, 2000, 4000, 8000, 16000, 32000, 64000),
  y_limits = c(10, 90),
  y_major_breaks = seq(10, 90, 10)
) +
#scale_size_continuous(range = c(1,30)) +
ggplot2::discrete_scale(
  scale_name = "continent_scale",
  name = "Continent",
  palette = palette_func,
  aesthetics = "fill"
) +
ggplot2::scale_size_continuous(
  name = "Population",
  range = c(1, 10)
) +
ggplot2::guides(
  fill = guide_legend(
    override.aes = list(size = 6, shape = 21)
  )
) +
theme(
  plot.margin = unit(rep(1, 4), "cm")
)
