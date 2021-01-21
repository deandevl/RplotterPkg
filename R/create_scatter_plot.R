#' Function produces a ggplot2 based scatter plot.
#'
#' @description Function returns a ggplot2 plot object of x/y scatter points/lines. Options are provided for scaling and
#'  line connection.
#'
#' @param df The target data frame from which the scatter points are plotted.
#' @param aes_x A string that sets the x axis variable name from \code{df}. Is a required discrete/continuous numeric,
#'  Date/POSIXct variable.
#' @param aes_y A string that sets the y axis variable name from \code{df}. Is a required discrete/continuous numeric.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill A string that sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param aes_size A string that sets the variable name from \code{df} for the aesthetic mapping for size.
#' @param aes_label A string that sets the variable name from \code{df} for the aesthetic mapping for labeling. If
#'  labeling points then the package \code{ggrepel} is required.
#' @param position A string or function that does a slight adjustment to overlapping points.  Typical values are
#'  \code{jitter} or \code{position_jitter(width = 0.1, height = 0.1)}.
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
#' @param x_major_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param x_minor_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines
#'  the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as \code{x_major_breaks}, that labels the major tics.
#' @param x_major_date_breaks If the class of \code{aes_x} is Date/POSIXct, a string containing the number and date unit
#'  for major breaks. \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_minor_date_breaks If the class of \code{aes_x} is Date/POSIXct, a string containing the number and date unit
#'  for minor breaks.
#' @param x_date_labels If the class of \code{aes_x} is Date/POSIXct, a string containing the format codes, the strftime
#'  format, for the date.
#'  Examples: \code{\%Y-\%m}, \code{\%Y/\%b/\%d}, \code{\%H-\%M-\%S}
#' @param x_log10 A logical which if \code{TRUE} will use a log10 scale for the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param y_log10 A logical which if \code{TRUE} will use a log10 scale for the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param show_pts A logical which if FALSE will plot only the lines if \code{connect} is TRUE.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_size A numeric value that sets the size of the points.
#' @param pts_line_alpha A numeric value that sets the alpha level of points and connected line.
#' @param palette_colors A character vector to set the palette colors.
#' @param connect A logical which if \code{TRUE} then points will be connected with a line.
#' @param line_size A numeric value that sets the thickness of lines if \code{connect} is TRUE.
#' @param line_color A string that sets the color of the lines if \code{connect} is TRUE.
#' @param connect_linetype A string that sets line type \code{twodash, solid, longdash, dotted, dotdash,
#'  dashed, blank} if \code{connect} is TRUE.
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param bold_y A numeric that plots a bold horizontal line at this y value intercept.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#' are removed.
#'
#' @importFrom rlang sym
#' @importFrom ggrepel geom_text_repel
#' @import ggplot2
#'
#' @return A plot object
#'
#' @author Rick Dean
#'
#' @export
create_scatter_plot <- function(
  df,
  aes_x,
  aes_y,
  aes_color = NULL,
  aes_fill = NULL,
  aes_size = NULL,
  aes_label = NULL,
  position = position_jitter(width = 0.0, height = 0.0),
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_title = aes_x,
  y_title = aes_y,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_minor_breaks = waiver(),
  x_labels = waiver(),
  x_major_date_breaks = waiver(),
  x_minor_date_breaks = waiver(),
  x_date_labels = waiver(),
  x_log10 = FALSE,
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  y_log10 = FALSE,
  axis_text_size = 11,
  show_pts = TRUE,
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  pts_color = "black",
  pts_line_alpha = 1.0,
  pts_size = 1,
  palette_colors = NULL,
  connect = FALSE,
  line_size = 1,
  line_color = "black",
  connect_linetype = "solid",
  do_x_title = TRUE,
  do_y_title = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_legend = TRUE,
  legend_pos = "top",
  bold_y = NULL,
  silent_NA_warning = FALSE) {
  aplot <- ggplot(data = df, aes(x = !!rlang::sym(aes_x), y = !!rlang::sym(aes_y)))

  if(!is.null(aes_color)) {
    if(!connect) {
      aplot <- aplot + geom_point(
        position = position,
        aes(color = !!sym(aes_color)),
        fill = pts_fill,
        size = pts_size,
        shape = pts_shape,
        stroke = pts_stroke,
        alpha = pts_line_alpha,
        na.rm = silent_NA_warning)
    }else {
      aplot <- aplot + geom_line(
        aes(color = !!sym(aes_color)),
        size = line_size,
        linetype = connect_linetype,
        alpha = pts_line_alpha,
        na.rm = silent_NA_warning)
      if(show_pts){
        aplot <- aplot + geom_point(
          position = position,
          aes(color = !!sym(aes_color)),
          fill = pts_fill,
          size = pts_size,
          shape = pts_shape,
          stroke = pts_stroke,
          alpha = pts_line_alpha,
          na.rm = silent_NA_warning
        )
      }
    }
  }else if(!is.null(aes_fill)){
    if(!connect){
      aplot <- aplot + geom_point(
        position = position,
        aes(fill = !!sym(aes_fill)),
        color = pts_color,
        size = pts_size,
        shape = pts_shape,
        stroke = pts_stroke,
        alpha = pts_line_alpha,
        na.rm = silent_NA_warning)
    }else {
      aplot <- aplot + geom_line(
        color = line_color,
        size = line_size,
        linetype = connect_linetype,
        alpha = pts_line_alpha,
        na.rm = silent_NA_warning)
      if(show_pts){
        aplot <- aplot + geom_point(
          position = position,
          aes(fill = !!sym(aes_fill)),
          color = pts_color,
          size = pts_size,
          shape = pts_shape,
          stroke = pts_stroke,
          alpha = pts_line_alpha,
          na.rm = silent_NA_warning
        )
      }
    }
  }else if(!is.null(aes_size)){
    if(!connect){
      aplot <- aplot + geom_point(
        position = position,
        aes(size = !!sym(aes_size)),
        fill = pts_fill,
        color = pts_color,
        shape = pts_shape,
        stroke = pts_stroke,
        alpha = pts_line_alpha,
        na.rm = silent_NA_warning)
    }else {
      aplot <- aplot + geom_line(
        size = line_size,
        color = line_color,
        linetype = connect_linetype,
        alpha = pts_line_alpha,
        na.rm = silent_NA_warning)
      if(show_pts){
        aplot <- aplot + geom_point(
          position = position,
          aes(size = !!sym(aes_size)),
          color = pts_color,
          fill = pts_fill,
          shape = pts_shape,
          stroke = pts_stroke,
          alpha = pts_line_alpha,
          na.rm = silent_NA_warning
        )
      }
    }
  }else {
    if(!connect && show_pts){
      aplot <- aplot + geom_point(
        position = position,
        fill = pts_fill,
        color = pts_color,
        size = pts_size,
        shape = pts_shape,
        stroke = pts_stroke,
        alpha = pts_line_alpha,
        na.rm = silent_NA_warning)
    }else {
      if(connect){
        aplot <- aplot + geom_line(
          color = line_color,
          size = line_size,
          linetype = connect_linetype,
          alpha = pts_line_alpha,
          na.rm = silent_NA_warning)
        }
      if(show_pts){
        aplot <- aplot + geom_point(
          fill = pts_fill,
          color = pts_color,
          size = pts_size,
          shape = pts_shape,
          stroke = pts_stroke,
          alpha = pts_line_alpha,
          na.rm = silent_NA_warning
        )
      }
    }
  }

  if(!is.null(aes_label)){
    aplot <- aplot +
      ggrepel::geom_text_repel(aes(label = !!sym(aes_label)))
  }

  if(!is.null(palette_colors)){
    if(!is.null(aes_fill)) {
      aplot <- aplot + scale_fill_manual(values = palette_colors)
    }else if(!is.null(aes_color)) {
      aplot <- aplot + scale_color_manual(values = palette_colors)
    }
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
  if(x_log10){
    aplot <- aplot + scale_x_log10(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = x_labels
    )
  }else if(is(df[[aes_x]], "Date")) {
    aplot <- aplot + scale_x_date(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = x_labels,
      date_breaks = x_major_date_breaks,
      date_minor_breaks = x_minor_date_breaks,
      date_labels = x_date_labels
    )
  }else if(is(df[[aes_x]],"POSIXct") || is(df[[aes_x]],"POSIXlt")) {
    aplot <- aplot + scale_x_datetime(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = x_labels,
      date_breaks = x_major_date_breaks,
      date_minor_breaks = x_minor_date_breaks,
      date_labels = x_date_labels
    )
  }else if(is(df[[aes_x]], "difftime") || is(df[[aes_x]], "hms")){
    aplot <- aplot + scale_x_time(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
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
        minor_breaks = x_minor_breaks,
        labels = x_labels
      )
    }
  }

  if(y_log10){
    aplot <- aplot + scale_y_log10(
      limits = y_limits,
      breaks = y_major_breaks,
      minor_breaks = y_minor_breaks,
      labels = y_labels
    )
  } else {
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
