#' Function produces a ggplot2 based "stick" plot.
#'
#' @description Function returns a plot object showing vertical/horizontal lines that run from a base value to
#' a measurement value. Options are provided for scaling.
#'
#' @param df The target data frame from which the \dQuote{stick} lines are drawn.
#' @param base_val A numeric that sets the base value from which the \dQuote{stick} originates.
#'  The default value is 0.
#' @param aes_x A string that sets the x axis variable name from \code{df}.  Can be a numeric/Date/POSIXct variable.
#' @param aes_y A string that sets the y axis variable name from \code{df} and controls the height of
#'  individual \dQuote{sticks}.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that
#'  defines the exact major tic locations along the x axis.
#' @param x_labels A character vector with the same length as \code{x_major_breaks}, that labels the major tics.
#' @param x_major_date_breaks If the class of \code{aes_x} is Date/POSIXct, a string containing the number and date
#'  unit for major breaks. \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels If the class of \code{aes_x} is Date/POSIXct, a string containing the format codes, the
#'  strftime format, for the date. Examples: \code{\%Y-\%m}, \code{\%Y/\%b/\%d}, \code{\%H-\%M-\%S}
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param line_color A string that sets the color of the lines.
#' @param line_size A numeric value that sets the thickness of lines.
#' @param line_alpha A numeric value that sets the degree of color alpha for the lines.
#' @param palette_colors A character vector to set the palette colors.
#' @param do_x_title A logical that controls the appearence of the x axis title.
#' @param do_y_title A logical that controls the appearence of the y axis title.
#' @param show_major_grids A logical that controls the appearence of major grids.
#' @param show_minor_grids A logical that controls the appearence of minor grids.
#' @param show_legend A logical that controls the appearence of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param bold_y A numeric that plots a bold horizontal line at this y value.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when NA's
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
create_stick_plot <- function(
  df,
  base_val = 0,
  aes_x = NULL,
  aes_y = NULL,
  aes_color = NULL,
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_title = aes_x,
  y_title = aes_y,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_labels = waiver(),
  x_major_date_breaks = waiver(),
  x_date_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  line_color = "black",
  line_size = 0.8,
  line_alpha = 1.0,
  palette_colors = NULL,
  do_x_title = TRUE,
  do_y_title = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_legend = TRUE,
  legend_pos = "top",
  bold_y = NULL,
  silent_NA_warning = FALSE) {

  aplot <- ggplot(data = df, aes(x = !!rlang::sym(aes_x)))
  if(!is.null(aes_color)) {
    aplot <- aplot + geom_linerange(
      aes(ymin = base_val, ymax = !!rlang::sym(aes_y), color = !!rlang::sym(aes_color)),
      size = line_size,
      alpha = line_alpha,
      na.rm = silent_NA_warning
    )
  }else {
    aplot <- aplot + geom_linerange(
      aes(ymin = base_val, ymax = !!rlang::sym(aes_y)),
      size = line_size,
      alpha = line_alpha,
      color = line_color,
      na.rm = silent_NA_warning)
  }

  if(!is.null(palette_colors) && !is.null(aes_color)){
    aplot <- aplot + scale_color_manual(values = palette_colors)
  }

  if(!show_legend){
    aplot <- aplot +
      theme(
        legend.position = "none"
      )
  }else {
    aplot <- aplot +
      theme(
        legend.position = legend_pos
      )
  }

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

  if(!is.null(bold_y)) {
    aplot <- aplot +
      geom_hline(aes(yintercept = bold_y), lwd = 1.2, linetype = "solid", color = "black")
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

  if(is(df[[aes_x]], "Date")) {
    aplot <- aplot + scale_x_date(
      limits = x_limits,
      breaks = x_major_breaks,
      labels = x_labels,
      date_breaks = x_major_date_breaks,
      date_labels = x_date_labels
    )
  }else if(is(df[[aes_x]],"POSIXct") || is(df[[aes_x]],"POSIXlt")) {
    aplot <- aplot + scale_x_datetime(
      limits = x_limits,
      breaks = x_major_breaks,
      labels = x_labels,
      date_breaks = x_major_date_breaks,
      date_labels = x_date_labels
    )
  }else if(is(df[[aes_x]], "difftime") || is(df[[aes_x]], "hms")){
    aplot <- aplot + scale_x_time(
      limits = x_limits,
      breaks = x_major_breaks,
      labels = x_labels
    )
  }else {
    if(is.factor(df[[aes_x]])){
      aplot <- aplot + scale_x_discrete(
        limits = x_limits,
        breaks = x_major_breaks,
        labels = x_labels
      )
    }else {
      aplot <- aplot + scale_x_continuous(
        limits = x_limits,
        breaks = x_major_breaks,
        labels = x_labels
      )
    }
  }

  if(is.factor(df[[aes_y]])){
    aplot <- aplot + scale_y_discrete(
      limits = y_limits,
      breaks = y_major_breaks,
      labels = y_labels
    )
  }else {
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
  if(do_x_title){
    aplot <- aplot +
      labs(x = x_title)
  }else {
    aplot <- aplot +
      theme(
        axis.title.x = element_blank()
      )
  }
  if(do_y_title){
    aplot <- aplot +
      labs(y = y_title)
  }else {
    aplot <- aplot +
      theme(
        axis.title.y = element_blank()
      )
  }

  return(aplot)
}
