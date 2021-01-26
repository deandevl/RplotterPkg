#' Function produces a ggplot2 based x/y plot of numeric ranges.
#'
#' @description Function returns a ggplot2 plot object displaying the individual
#'  spread or vertical interval/range for a collection of x/y pairs of points.
#'
#' @param df The target data frame from which the point ranges are plotted.
#' @param aes_x A string that sets the x axis variable name from \code{df}.
#'  It is a factor type variable that is associated with \code{aes_y}.
#' @param aes_y A string that sets a y axis variable name from \code{df}.
#'  These are the required numeric values that defines the location of the range
#'  on the y axis.
#' @param aes_y_min A string that sets a y axis variable name from \code{df}.
#'  These are the required numerics that defines the minimum values for the range
#'  of \code{aes_y}.
#' @param aes_y_max A string that sets a y axis variable name from \code{df}.
#'  These are the required numerics that defines the maximum values for the range
#'  of \code{aes_y}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title}
#'  and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels.
#'  When x tic labels are long, a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees
#'  for enhanced readability.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param y_log10 A logical which if \code{TRUE} will use a log10 scale for the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param line_type A string that sets range line type \code{twodash, solid, longdash, dotted, dotdash,
#'  dashed, blank}.
#' @param line_pts_size A numeric value that sets the size of both lines(width) and points(diameter).
#' @param line_pts_color A string that sets the color of the range lines and outlines of the points.
#' @param line_pts_alpha A numeric value that sets the alpha level of points.
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#' are removed.
#'
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @return A plot object
#'
#' @author Rick Dean
#'
#' @export
create_range_plot <- function(
  df,
  aes_x,
  aes_y,
  aes_y_min,
  aes_y_max,
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_title = NULL,
  y_title = NULL,
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
  line_pts_size = 1,
  line_pts_color = "black",
  line_pts_alpha = 1.0,
  do_x_title = TRUE,
  do_y_title = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  do_coord_flip = FALSE,
  silent_NA_warning = FALSE
){
  aplot <- ggplot(data = df, aes(x = !!rlang::sym(aes_x), y = !!rlang::sym(aes_y)))
  aplot <- aplot + geom_pointrange(
    aes(ymin = !!rlang::sym(aes_y_min), ymax = !!rlang::sym(aes_y_max)),
    color = line_pts_color,
    fill = pts_fill,
    shape = pts_shape,
    stroke = pts_stroke,
    linetype = line_type,
    size = line_pts_size,
    alpha = line_pts_alpha
  )

  aplot <- aplot +
    theme(
      panel.background = element_rect(fill = "white", color = "black")
    )

  if(show_major_grids){
    aplot <- aplot +
      theme(
        panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray")
      )
  }
  if(show_minor_grids){
    aplot <- aplot +
      theme(
        panel.grid.minor = element_line(size = 0.5, linetype = "solid", color = "gray")
      )
  }

  if(center_titles) {
    aplot <- aplot +
      theme(
        plot.title = element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = .5, size = 14)
      )
  }else {
    aplot <- aplot +
      theme(
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14)
      )
  }

  if(y_log10){
    aplot <- aplot + scale_y_log10(
      limits = y_limits,
      breaks = y_major_breaks,
      minor_breaks = y_minor_breaks,
      labels = y_labels
    )
  } else {
    aplot <- aplot + scale_y_continuous(
        limits = y_limits,
        breaks = y_major_breaks,
        minor_breaks = y_minor_breaks,
        labels = y_labels
      )
  }

  if(rot_y_tic_label){
    rot_y_tic_angle <- 0
  }else{
    rot_y_tic_angle <- 90
  }
  aplot <- aplot +
    theme(
      axis.text.x = element_text(size = axis_text_size, color = "black"),
      axis.title.x = element_text(size = axis_text_size + 2, color = "black"),
      axis.text.y = element_text(size = axis_text_size, color = "black", angle = rot_y_tic_angle),
      axis.title.y = element_text(size = axis_text_size + 2, color = "black")
    )
  if(rot_x_tic_angle > 0){
    aplot <- aplot +
      theme(axis.text.x = element_text(angle = rot_x_tic_angle, hjust = 1.0))
  }

  aplot <- aplot + labs(title = title, subtitle = subtitle)
  if(do_x_title & !is.null(x_title)){
    aplot <- aplot +
      labs(x = x_title)
  }else {
    aplot <- aplot +
      theme(
        axis.title.x = element_blank()
      )
  }
  if(do_y_title & !is.null(y_title)){
    aplot <- aplot +
      labs(y = y_title)
  }else {
    aplot <- aplot +
      theme(
        axis.title.y = element_blank()
      )
  }

  if(do_coord_flip){
    aplot <- aplot + coord_flip()
  }

  return(aplot)
}
