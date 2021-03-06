% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_histogram_plot.R
\name{multi_histogram_plot}
\alias{multi_histogram_plot}
\title{Produces multiple histogram plots.}
\usage{
multi_histogram_plot(
  df,
  variables = NULL,
  factor_var = NULL,
  factor_x = NULL,
  columns = 2,
  aes_color = NULL,
  aes_fill = NULL,
  position = "stack",
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_titles = NULL,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  bins = 20,
  binwidth = NULL,
  bin_breaks = NULL,
  bar_fill = "gray",
  bar_color = "black",
  bar_alpha = 1,
  bar_size = 1,
  palette_colors = NULL,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_minor_breaks = waiver(),
  x_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  col_width = 4,
  row_height = 4,
  do_coord_flip = FALSE,
  bar_labels = FALSE,
  bar_label_size = 6,
  bar_label_color = "black",
  plot_obs = FALSE,
  plot_obs_len = 0.02,
  plot_obs_color = "black",
  add_ons = NULL,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_legend = FALSE,
  legend_pos = "top",
  silent_NA_warning = FALSE,
  display_plot = TRUE
)
}
\arguments{
\item{df}{The source data frame for the histograms.}

\item{variables}{A vector that names the variables from \code{df} for plotting histograms.}

\item{factor_var}{A string that sets the factor variable from \code{df} with multiple levels.}

\item{factor_x}{A string that sets a variable name from \code{df}.  If \code{factor_var} is not NULL
then a histogram of \code{factor_x} will be plotted for each of \code{factor_var}'s levels.}

\item{columns}{An integer the sets the number of columns for the multipanel display.}

\item{aes_color}{A string that sets the variable name from \code{df} for the aesthetic mapping for color.}

\item{aes_fill}{A string that sets the variable name from \code{df} for the aesthetic mapping for fill.}

\item{position}{A string that sets the bar positions.  Acceptable values are \code{dodge}(side by side),
\code{identity}(overlap) or \code{stack}.}

\item{title}{A string that sets the overall title.}

\item{subtitle}{A string that sets the overall subtitle.}

\item{x_title}{A string that sets the x axis title.}

\item{y_titles}{A string vector that sets the y axis title cooresponding to each variable in \code{variables} or
each factor level in \code{factor_var}. If \code{y_titles} is \code{NULL} then the y axis title is \dQuote{Count}.}

\item{rot_x_tic_angle}{A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
a value of 40 for this argument usually works well.}

\item{rot_y_tic_label}{A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.}

\item{bins}{An integer that sets the number of bins for the histogram}

\item{binwidth}{A numeric that sets the bin width of the histogram.}

\item{bin_breaks}{A numeric vector giving the bin boundaries. Overrides \code{binwidth} and \code{bins}}

\item{bar_fill}{A strng that sets the fill color for the bars.}

\item{bar_color}{A string that sets the outline color for the bars.}

\item{bar_alpha}{A numeric that set the alpha component to \code{bar_color}.}

\item{bar_size}{A numeric that sets the outline thickness of the bars.}

\item{palette_colors}{A character vector to set the palette colors.}

\item{x_limits}{A numeric 2 element vector or function that sets the minimum and maximum for the x axis.  Use NA to
refer to the existing minimum and maximum.}

\item{x_major_breaks}{A numeric vector or function that sets the major tic locations along the x axis.}

\item{x_minor_breaks}{A numeric vector or function that sets the minor tic locations along the x axis.}

\item{x_labels}{A character vector or function giving x axis tic labels.  Must be the same length as \code{x_breaks}.}

\item{y_limits}{A numeric 2 element vector or function that sets the minimum and maximum for the y axis.  Use NA to
refer to the existing minimum and maximum.}

\item{y_major_breaks}{A numeric vector or function that sets the major tic locations along the y axis.}

\item{y_minor_breaks}{A numeric vector or function that sets the minor tic locations along the y axis.}

\item{y_labels}{A character vector or function giving y axis tic labels.  Must be the same length as \code{y_breaks}.}

\item{axis_text_size}{A numeric that sets the font size along the axis'. Default is 11.}

\item{col_width}{An integer that sets the width of each plot column in inches.}

\item{row_height}{An integer that sets the height of each plot column in inches.}

\item{do_coord_flip}{A logical which if TRUE will flip the x and y axis'.}

\item{bar_labels}{A logical which if \code{TRUE} will label each bar with its value.}

\item{bar_label_size}{A numeric that sets the size of the bar labels}

\item{bar_label_color}{A string that sets the color of the bar labels}

\item{plot_obs}{A logical which if \code{TRUE} plots a line for each observation along the axis margin.}

\item{plot_obs_len}{A numeric that sets the length of the \code{plot_obs} lines.}

\item{plot_obs_color}{A string that sets the color of the \code{plot_obs} lines.}

\item{add_ons}{Is a vector of ggplot objects that will be appended to separate scatter plots. Each
element in this vector addresses a separate plot in the order of presentation.}

\item{show_legend}{A logical that controls the appearance of the legend.}

\item{legend_pos}{A string that sets the legend position. Acceptable values are
\dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.}

\item{silent_NA_warning}{A logical that controls the appearance of a console warning when NA's
are removed.}

\item{display_plot}{A logical that if TRUE will display the plot}
}
\value{
A TableGrob object..
}
\description{
The function creates multiple histogram plots from one of two sources:
\enumerate{
\item From a vector containing selected names of a data frame's variables.
\item A single variable's histogram plot mapped across another variable's factor levels.
}

The function returns a single multi-panelled plot object for display.
}
\author{
Rick Dean
}
