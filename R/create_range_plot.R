#' @title create_range_plot
#' @description Function wraps ggplot2 geom_pointrange to produce x/y plot of numeric ranges.
#'  Function returns a ggplot2 plot object displaying the individual
#'  spread or vertical interval/range for a collection of x/y pairs of points.
#'
#' @param df The target data frame from which the point ranges are plotted.
#' @param aes_x Sets the x axis variable name from \code{df}.
#'  It is a required factor type variable that is associated with \code{aes_y}.
#' @param aes_y Sets a y axis variable name from \code{df}.
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
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title}
#'  and \code{subtitle}.
#' @param x_title A string that sets the x axis title. If NULL (the default)  then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If NULL (the default)  then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
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
#' @param line_width A numeric that sets the width of the lines.
#' @param fatten_pts A multiplicative numeric that sets the size of points(diameter).
#' @param line_pts_color A string that sets the color of the range lines and outlines of the points.
#' @param line_pts_alpha A numeric value that sets the alpha level of points.
#' @param panel_color A string in hexidecimal or color name that sets the plot panel's color.
#'   The default is "white".
#' @param panel_border_color A string in hexidecimal or color name that sets the plot panel's border color.
#'   The default is "black".
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
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
#' library(data.table)
#' library(ggplot2)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_range_plot(
#'   df = RplotterPkg::penguins_stats,
#'   aes_x = "species",
#'   aes_y = "avg_body_mass",
#'   aes_y_min = "min_body_mass",
#'   aes_y_max = "max_body_mass",
#'   title = "Average and Range of Penguins Body Mass(g) by Species",
#'   subtitle = "Source: palmerpenguins",
#'   center_titles = TRUE,
#'   x_title = "Species",
#'   y_title = "Body Mass(g)",
#'   rot_y_tic_label = TRUE,
#'   pts_fill = "blue",
#'   pts_shape = 22,
#'   pts_stroke = 1.7,
#'   line_width = 1.5,
#'   fatten_pts = 6,
#'   line_type = "solid",
#'   line_pts_color = "red",
#'   line_pts_alpha = 0.5,
#'   y_limits = c(2500, 7000),
#'   y_major_breaks = seq(2500,7000,500),
#'   show_major_grids = TRUE,
#'   show_minor_grids = FALSE,
#'   do_coord_flip = TRUE
#' )
#'
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @export
create_range_plot <- function(
    df = NULL,
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
    line_pts_alpha = 1.0,
    panel_color = "white",
    panel_border_color = "black",
    show_major_grids = TRUE,
    show_minor_grids = TRUE,
    do_coord_flip = FALSE,
    silent_NA_warning = FALSE,
    png_file_path = NULL,
    png_width_height = c(480,480)
){
  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
  }
  if(is.null(aes_x) | is.null(aes_y)){
    stop("Both aes_x and aes_y are required arguments.")
  }
  if(is.null(aes_y_min) | is.null(aes_y_max)){
    stop("Both aes_y_min and aes_y_max are required arguments.")
  }

  if(!is.null(aes_y_min)){
    aes_y_min <- rlang::sym(aes_y_min)
  }
  if(!is.null(aes_y_max)){
    aes_y_max <- rlang::sym(aes_y_max)
  }

  # -----------------Set geom attribute default values---------------
  ggplot2::update_geom_defaults(
    "pointrange",
    list(
      fill = pts_fill,
      shape = pts_shape,
      stroke = pts_stroke,
      color = line_pts_color,
      linewidth = line_width,
      fatten = fatten_pts,
      alpha = line_pts_alpha,
      linetype = line_type
    )
  )

  # -------------------Define the main ggplot2 plot object/geoms-----------
  aplot <- ggplot(data = df) +
    geom_pointrange(
      aes(
        x = !!sym(aes_x),
        y = !!sym(aes_y),
        ymin = !!aes_y_min,
        ymax = !!aes_y_max
      ),
      na.rm = silent_NA_warning
    )

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

  # Are we flipping axes?
  if(do_coord_flip){
    aplot <- aplot + coord_flip()
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

  # -------------------y axis tic scales------------------
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
