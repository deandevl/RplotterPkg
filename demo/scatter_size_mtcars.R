library(ggplot2)
library(RplotterPkg)

RplotterPkg::create_scatter_plot(
  df = mtcars,
  aes_x = "disp",
  aes_y = "hp",
  aes_fill = "mpg",
  aes_size = "carb",
  rot_y_tic_label = TRUE,
  x_title = "Engine Displacement",
  y_title = "Engine Horsepower",
  title = "Engine Displacement Versus Horsepower Across Levels of MPG",
  subtitle = "Source: datasets::mtcars",
  x_limits = c(50, 500),
  x_major_breaks = seq(50, 500, 50),
  y_limits = c(50, 400),
  y_major_breaks = seq(50, 400, 50)
) +
ggplot2::scale_size_continuous(
  name = "Number of Carbs",
  range = c(1,15)
) +
ggplot2::scale_fill_gradientn(
  limits = c(10, 40),
  breaks = seq(10, 40, 5),
  colors = c(red = "#E62600", white = "#FFFFFF", green = "#44CC00"),
  guide = "legend"
) +
ggplot2::guides(
  fill = guide_legend(
    title = "Miles Per Gallon",
    override.aes = list(size = 6)
  )
)
