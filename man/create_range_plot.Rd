% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_range_plot.R
\name{create_range_plot}
\alias{create_range_plot}
\title{Function wraps ggplot2 geom_pointrange to produce x/y plot of numeric ranges.}
\usage{
create_range_plot(
  df,
  aes_x = NULL,
  aes_y = NULL,
  aes_y_min = NULL,
  aes_y_max = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  center_titles = FALSE,
  x_title = NULL,
  y_title = NULL,
  hide_x_tics = FALSE,
  hide_y_tics = FALSE,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  y_log10 = FALSE,
  axis_text_size = 11,
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  line_type = "solid",
  line_width = 1,
  fatten_pts = 4,
  line_pts_color = "black",
  line_pts_alpha = 1,
  panel_color = "white",
  panel_border_color = "black",
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  do_coord_flip = FALSE,
  silent_NA_warning = FALSE,
  png_file_path = NULL,
  png_width_height = c(480, 480)
)
}
\arguments{
\item{df}{The target data frame from which the point ranges are plotted.}

\item{aes_x}{Sets the x axis variable name from \code{df}.
It is a factor type variable that is associated with \code{aes_y}.}

\item{aes_y}{Sets a y axis variable name from \code{df}.
These are the required numeric values that defines the location of the range
on the y axis.}

\item{aes_y_min}{A string that sets a y axis variable name from \code{df}.
These are the required numerics that defines the minimum values for the range
of \code{aes_y}.}

\item{aes_y_max}{A string that sets a y axis variable name from \code{df}.
These are the required numerics that defines the maximum values for the range
of \code{aes_y}.}

\item{title}{A string that sets the plot title.}

\item{subtitle}{A string that sets the plot subtitle.}

\item{caption}{A string that sets the plot caption}

\item{center_titles}{A logical which if \code{TRUE} centers both the \code{title}
and \code{subtitle}.}

\item{x_title}{A string that sets the x axis title. If NULL (the default)  then the x axis title does not appear.}

\item{y_title}{A string that sets the y axis title. If NULL (the default)  then the y axis title does not appear.}

\item{hide_x_tics}{A logical that controls the appearance of the x axis tics.}

\item{hide_y_tics}{A logical that controls the appearance of the y axis tics.}

\item{rot_x_tic_angle}{A numeric that sets the angle of rotation for the x tic labels.
When x tic labels are long, a value of 40 for this argument usually works well.}

\item{rot_y_tic_label}{A logical which if TRUE rotates the y tic labels 90 degrees
for enhanced readability.}

\item{y_limits}{A numeric 2 element vector that sets the minimum and  maximum for the y axis.
Use \code{NA} to refer to the existing minimum and maximum.}

\item{y_major_breaks}{A numeric vector or function that defines the exact major tic locations along the y axis.}

\item{y_minor_breaks}{A numeric vector or function that defines the exact minor tic locations along the y axis.}

\item{y_labels}{A character vector with the same length as \code{y_major_breaks}, that labels the major tics.}

\item{y_log10}{A logical which if \code{TRUE} will use a log10 scale for the y axis.}

\item{axis_text_size}{A numeric that sets the font size along the axis'. Default is 11.}

\item{pts_fill}{A string that sets the fill color of the points.}

\item{pts_shape}{A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.}

\item{pts_stroke}{A numeric that sets the drawing width for a point shape.}

\item{line_type}{A string that sets range line type \code{twodash, solid, longdash, dotted, dotdash,
dashed, blank}.}

\item{line_width}{A numeric that sets the width of the lines.}

\item{fatten_pts}{A multiplicative numeric that sets the size of points(diameter).}

\item{line_pts_color}{A string that sets the color of the range lines and outlines of the points.}

\item{line_pts_alpha}{A numeric value that sets the alpha level of points.}

\item{panel_color}{A string in hexidecimal or color name that sets the plot panel's color.
The default is "white".}

\item{panel_border_color}{A string in hexidecimal or color name that sets the plot panel's border color.
The default is "black".}

\item{show_major_grids}{A logical that controls the appearance of major grids.}

\item{show_minor_grids}{A logical that controls the appearance of minor grids.}

\item{do_coord_flip}{A logical which if \code{TRUE} will flip the x and y axis'.}

\item{silent_NA_warning}{A logical that controls the appearance of a console warning when Na's
are removed.}

\item{png_file_path}{A character string with the directory and file name to produce
a png image of the plot.}

\item{png_width_height}{A numeric vector that sets the width and height of the png image in pixels. The
default is c(480,480).  There are 37.8 pixels in a centimeter.}
}
\value{
A plot object
}
\description{
Function returns a ggplot2 plot object displaying the individual
spread or vertical interval/range for a collection of x/y pairs of points.
}
\author{
Rick Dean
}
