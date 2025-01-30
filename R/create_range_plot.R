#' @title create_range_plot
#'
#' @description Function wraps \code{ggplot2::geom_pointrange} to produce x/y plot of numeric ranges.
#'  Function returns a ggplot2 plot object displaying the individual
#'  spread or vertical interval/range for a collection of x/y pairs of points.
#'
#' @param df The target data frame from which the point ranges are plotted.
#' @param orientation A string that sets the axis on which the range should run along.
#'  Acceptable values are either "x" (the default) or "y".
#' @param aes_x Sets the x axis numeric variable name from 'df'.
#' @param aes_y Sets a y axis variable name from 'df'.
#' @param aes_min A string that sets a numeric variable from 'df' that defines
#'  the minimum values for the range.
#' @param aes_max A string that sets a numeric variable from 'df' that defines
#'  the maximum values for the range.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the 'title'
#'  and 'subtitle'.
#' @param x_title A string that sets the x axis title. If \code{NULL} (the default)  then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If \code{NULL} (the default)  then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels.
#'  When x tic labels are long, a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees
#'  for enhanced readability.
#' @param x_limits A numeric 2 element vector that sets the minimum and  maximum for the x axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param x_major_breaks A numeric vector or function that defines the exact major tic locations along the x axis.
#' @param x_minor_breaks A numeric vector or function that defines the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as 'x_major_breaks', that labels the major tics.
#' @param x_log10 A logical which if \code{TRUE} will use a log10 scale for the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as 'y_major_breaks', that labels the major tics.
#' @param y_log10 A logical which if \code{TRUE} will use a log10 scale for the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 "circle",
#'  22 "square", 23 "diamond", 24 "up triangle", 25 "down triangle".
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_size A numeric that sets the point size.
#' @param line_type A string that sets range line type "twodash", "solid", "longdash", "dotted", "dotdash",
#'  "dashed", "blank".
#' @param line_width A numeric that sets the width of the lines.
#' @param line_pts_color A string that sets the color of the range lines and outlines of the points.
#' @param line_pts_alpha A numeric value that sets the alpha level of points.
#' @param panel_color A string in hexidecimal or color name that sets the plot panel's color.
#'   The default is "white".
#' @param panel_border_color A string in hexidecimal or color name that sets the plot panel's border color.
#'   The default is "black".
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param bold_x A numeric that sets the x-intercept for plotting a bold vertical line.
#' @param bold_x_color A string that sets the color of 'bold_x'.
#' @param bold_x_linetype A string that set the linetype of 'bold_x'.
#' @param bold_y A numeric that sets the y-intercept for plotting a bold horizontal line.
#' @param bold_y_color A string that sets the color of 'bold_y'.
#' @param bold_y_linetype A string that set the linetype of 'bold_y'.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#' are removed.
#' @param png_file_path A character string with the directory and file name to produce
#'  a png image of the plot.
#' @param png_width_height A numeric vector that sets the width and height of the png image in pixels. The
#'  default is c(480,480).  There are 37.8 pixels in a centimeter.
#'
#' @return A ggplot class object.
#'
#' @examples
#' library(ggplot2)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_range_plot(
#'   df = RplotterPkg::penguins_stats,
#'   orientation = "x",
#'   aes_x = "avg_body_mass",
#'   aes_y = "species",,
#'   aes_min = "min_body_mass",
#'   aes_max = "max_body_mass",
#'   title = "Average and Range of Penguins Body Mass(g) by Species",
#'   subtitle = "Source: palmerpenguins",
#'   center_titles = TRUE,
#'   x_title = "Body Mass(g)",
#'   y_title = "Species",
#'   pts_fill = "blue",
#'   pts_shape = 22,
#'   pts_stroke = 1.7,
#'   line_width = 1.5,
#'   line_type = "solid",
#'   line_pts_color = "red",
#'   line_pts_alpha = 0.5,
#'   x_limits = c(2500, 7000),
#'   x_major_breaks = seq(2500,7000,500),
#'   show_major_grids = TRUE,
#'   show_minor_grids = FALSE
#' )
#'
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @export
create_range_plot <- function(
    df = NULL,
    orientation = "x",
    aes_x = NULL,
    aes_y = NULL,
    aes_min = NULL,
    aes_max = NULL,
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
    x_limits = NULL,
    x_major_breaks = waiver(),
    x_minor_breaks = waiver(),
    x_labels = waiver(),
    x_log10 = FALSE,
    y_limits = NULL,
    y_major_breaks = waiver(),
    y_minor_breaks = waiver(),
    y_labels = waiver(),
    y_log10 = FALSE,
    axis_text_size = 11,
    pts_fill = "white",
    pts_shape = 21,
    pts_stroke = 1,
    pts_size = 2,
    line_type = "solid",
    line_width = 1,
    line_pts_color = "black",
    line_pts_alpha = 1.0,
    panel_color = "white",
    panel_border_color = "black",
    show_major_grids = TRUE,
    show_minor_grids = TRUE,
    bold_x = NULL,
    bold_x_color = "black",
    bold_x_linetype = "dashed",
    bold_y = NULL,
    bold_y_color = "black",
    bold_y_linetype = "dashed",
    silent_NA_warning = FALSE,
    png_file_path = NULL,
    png_width_height = c(480,480)
){
  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
  }
  if(is.null(aes_x) | is.null(aes_y)){
    stop("Both aes_x and aes_y are required parameters.")
  }
  if(is.null(aes_min) | is.null(aes_max)){
    stop("Both aes_min and aes_max are required parameters.")
  }

  # -----------------Set geom attribute default values---------------
  ggplot2::update_geom_defaults(
    "point",
    list(
      fill = pts_fill,
      shape = pts_shape,
      stroke = pts_stroke,
      size = pts_size,
      color = line_pts_color,
      alpha = line_pts_alpha
    )
  )
  ggplot2::update_geom_defaults(
    "linerange",
    list(
      color = line_pts_color,
      linewidth = line_width,
      alpha = line_pts_alpha,
      linetype = line_type
    )
  )
  # -------------------Define the main ggplot2 plot object/geoms-----------
  aplot <- ggplot(data = df)
  aplot <- aplot +
    geom_point(
      aes(
        x = !!sym(aes_x),
        y = !!sym(aes_y),
      ),
      na.rm = silent_NA_warning
    )

  if(orientation == "x"){
    aplot <- aplot +
      geom_linerange(
        aes(
          y = !!sym(aes_y),
          xmin = !!sym(aes_min),
          xmax = !!sym(aes_max)
        ),
        na.rm = silent_NA_warning
      )
  }else if(orientation == "y"){
    aplot <- aplot +
      geom_linerange(
        aes(
          x = !!sym(aes_x),
          ymin = !!sym(aes_min),
          ymax = !!sym(aes_max)
        ),
        na.rm = silent_NA_warning
      )
  }

  # -------------------Additional ggplot2 components------------------------
  # ----------------------title and subtitle-----------------
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
  aplot <- aplot + labs(title = title, subtitle = subtitle, caption = caption)

  # --------------------panel and grids---------------------
  aplot <- aplot +
    theme(
      panel.background = element_rect(fill = panel_color, color = panel_border_color, linewidth = 2)
    )

  if(show_major_grids){
    aplot <- aplot +
      theme(
        panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", color = "gray")
      )
  }
  if(show_minor_grids){
    aplot <- aplot +
      theme(
        panel.grid.minor = element_line(linewidth = 0.5, linetype = "solid", color = "gray")
      )
  }

  # -------------------rotate/size tic labels--------------------
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

  # --------------------x/y axis titles------------------------
  if(is.null(x_title)) {
      aplot <- aplot +
        theme(
          axis.title.x = element_blank()
        )
    }else{
      aplot <- aplot +
        labs(x = x_title)
    }
    if(is.null(y_title)) {
      aplot <- aplot +
        theme(
          axis.title.y = element_blank()
        )
    }else{
      aplot <- aplot +
        labs(y = y_title)
    }

  # -------------------x/y axis tic scales------------------
  if(orientation == "y") {
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
  }else if(orientation == "x"){
    if(x_log10){
      aplot <- aplot + scale_x_log10(
        limits = x_limits,
        breaks = x_major_breaks,
        minor_breaks = x_minor_breaks,
        labels = x_labels
      )
    } else {
      aplot <- aplot + scale_x_continuous(
        limits = x_limits,
        breaks = x_major_breaks,
        minor_breaks = x_minor_breaks,
        labels = x_labels
      )
    }
  }

  # -----------------------hide x/y axis tics?----------------------
  if(hide_x_tics){
    aplot <- aplot +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }
  if(hide_y_tics){
    aplot <- aplot +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }

  if(!is.null(png_file_path)){
    grDevices::png(
      filename = png_file_path,
      width = png_width_height[[1]],
      height = png_width_height[[2]]
    )
    print(aplot)
    grDevices::dev.off()
  }

  # -------------add bold vertical/horizontal line if requested------------
  if(!is.null(bold_x)) {
    aplot <- aplot +
      geom_vline(data = df, aes(xintercept = bold_x), lwd = 1, linetype = bold_x_linetype, color = bold_x_color)
  }
  if(!is.null(bold_y)) {
    aplot <- aplot +
      geom_hline(data = df, aes(yintercept = bold_y), lwd = 1, linetype = bold_y_linetype, color = bold_y_color)
  }
  return(aplot)
}
