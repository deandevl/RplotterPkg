library(ggplot2)
library(rlang)
library(data.table)
library(RplotterPkg)

#  Create a data frame with a 1000 random observations
#    from a standard normal distributions:
set.seed(42)
data_df <- data.frame(
  x = stats::rnorm(n = 1000)
)

# Create a density plot of the random data with area shading
#    at cumulative probabilities of 2.5% and 97.5%:
random_normal_plot <- RplotterPkg::create_density_plot(
  df = data_df,
  aes_x = "x",
  title = "Random Normal Distribution",
  subtitle = "probability area shading",
  x_limits = c(-4,4),
  x_major_breaks = seq(-4,4,1),
  y_limits = c(0.0, 0.5),
  y_major_breaks = seq(0.0, 0.5, 0.1),
  rot_y_tic_label = TRUE,
 # cum_prob = c(0.025, 0.975),
 # area_colors = c("gray", "green", "gray"),
  cum_prob = 0.975,
  area_colors = c("green", "gray"),
  area_quantile_line_color = "blue"
)

random_normal_plot
