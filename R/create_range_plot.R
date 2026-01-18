#' @title create_range_plot
#'
#' @description Function creates a plot of scaled line segments from begin/end of continuous numeric values
#'  (referred to as 'measure') in either a vertical or horizontal orientation. The segments are positioned
#'  across the axis using discrete values or levels (referred to as 'factor'). For example, segment lines
#'  scaled with lower/upper values of temperature across months of a year.  In addition an optional mid-point
#'  such as a mean, median or difference between the lower/upper values can be plotted on the individual
#'  segment lines.
#'
#' @param df The target data frame from which the point ranges are plotted.
#' @param orientation A string that sets the orientation/direction of the segments.
#'  Acceptable values are either "vertical" (the default) or "horizontal".
#' @param factor_var A required variable name from 'df' that sets the factor/discreet variable for segment positioning.
#' @param min_meas A required variable name from 'df' that defines the minimum values for each segment.
#' @param max_meas A required variable name from 'df' that defines the maximum values for each segment.
#' @param labels_meas An optional variable name from 'df' that sets the value for a labelled point on each segment.
#' @param labels A character variable from 'df' that defines the actual label for each segment.
#' @param labels_fill A string that sets the fill color of the labelled point on the segments.
#' @param labels_hjust A numeric that adjust the 'labels' in the horizontal direction.
#' @param labels_vjust A numeric that adjust the 'labels' in the vertical direction.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the 'title'
#'  and 'subtitle'.
#' @param factor_title A string that sets the axis title the factor.
#' @param meas_title A string that sets the axis title for the measures.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels.
#'  When factor tic labels are long, a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees
#'  for enhanced readability.
#' @param meas_limits A numeric 2 element vector that sets the minimum and maximum for the measure axis.
#' @param meas_major_breaks A numeric vector or function that defines the exact major tic locations along the measure axis.
#' @param meas_minor_breaks A numeric vector or function that defines the exact minor tic locations along the measure axis.
#' @param meas_labels A character vector with the same length as 'meas_major_breaks', that labels the major tics.
#' @param meas_log10 A logical which if \code{TRUE} will use a log10 scale for the measure axis.
#' @param meas_padding A numeric 2 element vector that pads the measure axis scale.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 "circle",
#'  22 "square", 23 "diamond", 24 "up triangle", 25 "down triangle".
#' @param pts_stroke A numeric that sets the outline width for a point shape.
#' @param pts_stroke_color A string that sets the point's outline color.
#' @param pts_size A numeric that sets the point size.
#' @param pts_fill A string vector that sets the colors of the lower and upper points.
#'   The default is c("green", "red").
#' @param line_type A string that sets the segment line type "twodash", "solid", "longdash", "dotted", "dotdash",
#'  "dashed", "blank".
#' @param line_width A numeric that sets the width of the segments.
#' @param line_pts_alpha A numeric value that sets the alpha level of points.
#' @param panel_color A string in hexidecimal or color name that sets the plot panel's color.
#'   The default is "white".
#' @param panel_border_color A string in hexidecimal or color name that sets the plot panel's border color.
#'   The default is "black".
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
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
#'   orientation = "vertical",
#'   factor_var = "species",
#'   min_meas = "min_body_mass",
#'   max_meas = "max_body_mass",
#'   labels_meas = "avg_body_mass",
#'   labels = "mid_labels",
#'   labels_fill = "yellow",
#'   title = "Average and Range of Penguins Body Mass(g) by Species",
#'   subtitle = "Source: palmerpenguins",
#'   center_titles = TRUE,
#'   factor_title = "Species",
#'   meas_title = "Body Mass(g)",
#'   pts_fill = c("white", "red"),
#'   pts_size = 3,
#'   pts_shape = 21,
#'   pts_stroke = 1.7,
#'   line_width = 1.5,
#'   line_type = "solid",
#'   line_pts_alpha = 0.5,
#'   meas_limits = c(2500, 7000),
#'   meas_major_breaks = seq(2500,7000,500),
#'   rot_y_tic_label = TRUE,
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
    orientation = "vertical",
    factor_var = NULL,
    min_meas = NULL,
    max_meas = NULL,
    labels = NULL,
    labels_meas = NULL,
    labels_fill = "red",
    labels_hjust = -0.4,
    labels_vjust = 1.4,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    center_titles = FALSE,
    factor_title = NULL,
    meas_title = NULL,
    hide_x_tics = FALSE,
    hide_y_tics = FALSE,
    rot_x_tic_angle = 0,
    rot_y_tic_label = FALSE,
    meas_limits = NULL,
    meas_major_breaks = waiver(),
    meas_minor_breaks = waiver(),
    meas_labels = waiver(),
    meas_log10 = FALSE,
    meas_padding = c(.02,.02),
    axis_text_size = 11,
    pts_shape = 21,
    pts_stroke = 1,
    pts_stroke_color = "black",
    pts_size = 2,
    pts_fill = c("green","red"),
    line_type = "solid",
    line_width = 1,
    line_pts_alpha = 1.0,
    panel_color = "white",
    panel_border_color = "black",
    show_major_grids = TRUE,
    show_minor_grids = TRUE,
    silent_NA_warning = FALSE,
    png_file_path = NULL,
    png_width_height = c(480,480)
){
  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
  }
  if(is.null(min_meas) | is.null(max_meas)){
    stop("Both min_meas and max_meas are required parameters.")
  }
  if(is.null(factor_var)){
    stop("factor_var is a required parameter.")
  }

  # -----------------Set geom attribute default values---------------
  ggplot2::update_geom_defaults(
    "point",
    list(
      shape = pts_shape,
      stroke = pts_stroke,
      color = pts_stroke_color,
      size = pts_size,
      alpha = line_pts_alpha
    )
  )
  ggplot2::update_geom_defaults(
    "linerange",
    list(
      linewidth = line_width,
      alpha = line_pts_alpha,
      linetype = line_type
    )
  )
  # -------------------Define the main ggplot2 plot object/geoms-----------
  aplot <- ggplot(data = df)
  if(orientation == "horizontal"){
    aplot <- aplot +
    geom_point(
      aes(
        x = !!sym(min_meas),
        y = !!sym(factor_var)
      ),
      fill = pts_fill[[1]],
      na.rm = silent_NA_warning
    ) +
    geom_point(
      aes(
        x = !!sym(max_meas),
        y = !!sym(factor_var)
      ),
      fill = pts_fill[[2]],
      na.rm = silent_NA_warning
    ) +
    geom_linerange(
      aes(
        y = !!sym(factor_var),
        xmin = !!sym(min_meas),
        xmax = !!sym(max_meas)
      ),
      na.rm = silent_NA_warning
    )
    if(!is.null(labels_meas)){
      aplot <- aplot +
      geom_point(
        aes(
          x = !!sym(labels_meas),
          y = !!sym(factor_var)
        ),
        fill = labels_fill,
        na.rm = silent_NA_warning
      )
      if(!is.null(labels)){
        aplot = aplot + geom_text(
          aes(
            x = !!sym(labels_meas),
            y = !!sym(factor_var),
            label = !!sym(labels)
          ),
          vjust = labels_vjust,
          hjust = labels_hjust
        )
      }
    }

    # --------------------x/y axis titles------------------------
    aplot <- aplot +
      labs(x = meas_title)
    aplot <- aplot +
      labs(y = factor_title)

  }else if(orientation == "vertical"){
    aplot <- aplot +
    geom_point(
      aes(
        x = !!sym(factor_var),
        y = !!sym(min_meas)
      ),
      fill = pts_fill[[1]],
      na.rm = silent_NA_warning
    ) +
    geom_point(
      aes(
        x = !!sym(factor_var),
        y = !!sym(max_meas)
      ),
      fill = pts_fill[[2]],
      na.rm = silent_NA_warning
    ) +
    geom_linerange(
      aes(
        x = !!sym(factor_var),
        ymin = !!sym(min_meas),
        ymax = !!sym(max_meas)
      ),
      na.rm = silent_NA_warning
    )
    if(!is.null(labels_meas)){
      aplot <- aplot +
        geom_point(
          aes(
            x = !!sym(factor_var),
            y = !!sym(labels_meas)
          ),
          fill = labels_fill
        )
      if(!is.null(labels)){
        aplot = aplot + geom_text(
          aes(
            x = !!sym(factor_var),
            y = !!sym(labels_meas),
            label = !!sym(labels)
          ),
          hjust = labels_hjust,
          vjust = labels_vjust
        )
      }
    }

    # --------------------x/y axis titles------------------------
    aplot <- aplot +
      labs(x = factor_title)

    aplot <- aplot +
      labs(y = meas_title)
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

  # -------------------x/y axis tic scales------------------
  if(orientation == "vertical") {
    if(meas_log10){
      aplot <- aplot + scale_y_log10(
        limits = meas_limits,
        breaks = meas_major_breaks,
        minor_breaks = meas_minor_breaks,
        labels = meas_labels
      )
    } else {
      aplot <- aplot + scale_y_continuous(
        limits = meas_limits,
        breaks = meas_major_breaks,
        minor_breaks = meas_minor_breaks,
        labels = meas_labels,
        expand = expansion(meas_padding)
      )
    }
  }else if(orientation == "horizontal"){
    if(meas_log10){
      aplot <- aplot + scale_x_log10(
        limits = meas_limits,
        breaks = meas_major_breaks,
        minor_breaks = meas_minor_breaks,
        labels = meas_labels
      )
    } else {
      aplot <- aplot + scale_x_continuous(
        limits = meas_limits,
        breaks = meas_major_breaks,
        minor_breaks = meas_minor_breaks,
        labels = meas_labels,
        expand = expansion(meas_padding)
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

  return(aplot)
}
