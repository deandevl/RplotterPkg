library(data.table)
library(gapminder)
library(magrittr)
library(anytime)
library(ggplot2)
library(RColorBrewer)
library(RplotterPkg)

str(gapminder::gapminder)

# Set  with value of "Oceania" as "Asia"
gapminder_dt <- data.table::as.data.table(gapminder::gapminder) %>%
  .[, continent := as.character(continent)] %>%
  .[, continent_2 := ifelse(continent == "Oceania", "Asia", continent)] %>%
  .[, continent_2 := as.factor(continent_2)] %>%
  .[, year := as.character(year)] %>%
  .[, year := anytime::anydate(year)]

# Group by "year" and "continent_2" and compute the mean "lifeExp"
gapminder_mean_lifeExp_dt <- gapminder_dt[,
                                          lapply(.SD, mean),
                                          by = .(year, continent_2),
                                          .SDcols = "lifeExp"]

# Change columns names including "lifeExp" to "Mean LifeExp"
data.table::setnames(
  gapminder_mean_lifeExp_dt,
  old = c("year", "continent_2", "lifeExp"),
  new = c("Year", "Continent", "MeanLifeExp")
)

str(gapminder_mean_lifeExp_dt)

# Display the scatter plot
palette_func <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 4, name = "Set1"))

RplotterPkg::create_scatter_plot(
  df = gapminder_mean_lifeExp_dt,
  aes_x = "Year",
  aes_y = "MeanLifeExp",
  x_title = "Year",
  y_title = "MeanLifeExp",
  aes_fill = "Continent",
  aes_color = "Continent",
  pts_size = 5,
  connect = TRUE,
  x_limits = c(as.Date("1950-01-01"), as.Date("2010-01-01")),
  x_major_date_breaks = "5 year",
  x_date_labels = "%Y",
  legend_key_height = 1.0,
  title = "Life Expectancy Across Years and Continents",
  subtitle = "Source: gapminder::gapminder"
) +
ggplot2::discrete_scale(
  scale_name = "continent scale",
  name = "Continent",
  palette = palette_func,
  aesthetics = "fill"
)
