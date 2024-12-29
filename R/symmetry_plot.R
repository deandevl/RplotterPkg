#' @title symmetry_plot
#'
#' @description Function creates a ggplot2 based symmetry plot from a vector of numeric values.
#'
#' Function orders the numeric vector from low to high. It then divides the data into two groups: values above
#' the median(u group) and values below the median(v group). The distances from the median for values
#' in each group are computed. A scatter plot is created where distances in the v group are plotted along
#' the x axis and distances from the v group are plotted along the y axis. If the upper and lower distance
#' data forms a straight line then the original data is perfectly symmetrical.
#'
#' A reference symmetry line is also drawn. If points fall close to the line then the data is nearly
#' symmetric. If the data is left-skewed, then the points fall below the line. If the data is right
#' skewed, the points fall above the line.
#'
#' Function returns a ggplot2 plot object of x/y scatter point distances from the median. Options are provided
#' for axis scaling and point labelling.
#'
#' @param df The target data frame from which the distance from the median are plotted.
#' @param var_name A string that sets the variable name of interest from \code{df}. This is a required parameter.
#' @param position A string or function that does a slight adjustment to overlapping points.  Typical values are
#'  "jitter" or \code{position_jitter(width = 0.1, height = 0.1)}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title. If NULL (the default) then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If NULL (the default) then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Sets the minimum and maximum for the x axis.
#' @param x_major_breaks A numeric vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param x_minor_breaks A numeric vector or function that defines
#'  the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as \code{x_major_breaks}, that labels the major tics.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param x_y_decimals A two element numeric vector that set the number of decimals for the x and y tic labels.
#' @param x_y_scientific A two element logical vector that if TRUE uses scientific notation for the x and y tic labels.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_color A string that sets the color attribute of the points.
#' @param pts_fill A string that sets the fill color attribute of the points.
#' @param pts_shape A numeric integer that sets the shape attribute of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing stroke  width attribute for a point shape.
#' @param pts_size A numeric value that sets the size attribute of the points.
#' @param pts_alpha A numeric value that sets the alpha level attribute of points.
#' @param line_width A numeric value that sets the width of the symmetry line.
#' @param line_color A string that sets the color of the symmetry line.
#' @param line_alpha A numeric that sets the alpha of the symmetry line.
#' @param line_linetype A string that sets the symmetry line type \code{twodash, solid, longdash, dotted, dotdash,
#'  dashed, blank}.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param panel_color A string in hexidecimal or color name that sets the plot panel's color.
#'   The default is "white".
#' @param panel_border_color A string in hexidecimal or color name that sets the plot panel's border color.
#'   The default is "black".
#'
#' @return A ggplot class plot object
#'
#' @examples
#' library(ggplot2)
#' library(ggrepel)
#' library(RplotterPkg)
#'
#' RplotterPkg::symmetry_plot(
#'   df = RplotterPkg::farms,
#'   var_name = "count",
#'   title = "Symmetry in farm counts across US states",
#'   rot_y_tic_label = TRUE,
#'   line_linetype = "dotted"
#' )
#'
#' @import ggplot2
#' @importFrom stats median
#'
#' @export
symmetry_plot <- function(
  df,
  var_name,
  position = position_jitter(width = 0.0, height = 0.0),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  center_titles = FALSE,
  x_title = "V",
  y_title = "U",
  hide_x_tics = FALSE,
  hide_y_tics = FALSE,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_minor_breaks = waiver(),
  x_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  x_y_decimals = NULL,
  x_y_scientific = NULL,
  axis_text_size = 11,
  pts_fill = "black",
  pts_shape = 21,
  pts_stroke = 1.0,
  pts_color = "black",
  pts_size = 1.0,
  pts_alpha = 1.0,
  line_width = 0.6,
  line_color = "blue",
  line_alpha = 0.7,
  line_linetype = "solid",
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  panel_color = "white",
  panel_border_color = "black"
){
  # -----------------Set geom attribute default values---------------
  ggplot2::update_geom_defaults(
    "point",
    list(
      size = pts_size,
      fill = pts_fill,
      shape = pts_shape,
      stroke = pts_stroke,
      color = pts_color,
      alpha = pts_alpha
    )
  )

   # -------------------Define the main ggplot2 plot object/geoms-----------
  vals <- sort(df[[var_name]])
  M <- median(vals)
  n <- length(vals)
  n_1 <- n/2
  if(n %% 2 != 0){
    n_1 <- n_1 + 1
  }

  u <- rev(vals[(n_1+1):n] - M)
  v <- M - vals[1:n_1]

  median_diff_df <- data.frame(
    v = v,
    u = u
  )

  aplot <- ggplot(
    data = median_diff_df,
    aes(
      x = v,
      y = u,
    )
  )
  aplot <- aplot +
    geom_point(
      position = position
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

  # -------------------axis tic scaling------------------
  # define function for formatting decimals
  fmt_decimals <- function(decimals=0, sci=FALSE){
    function(x) {format(x,nsmall = decimals,scientific=sci)}
  }

  if(!is.null(x_y_decimals) | !is.null(x_y_scientific)) {
    if(is.null(x_y_decimals)){
      x_y_decimals <- c(0,0)
    }
    if(is.null(x_y_scientific)){
      x_y_scientific <- c(FALSE, FALSE)
    }

    aplot <- aplot + scale_x_continuous(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = fmt_decimals(x_y_decimals[[1]], x_y_scientific[[1]])
    )
  }else {
    aplot <- aplot + scale_x_continuous(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = x_labels
    )
  }

  # ---------------y axis tic scaling-------------------
  if(!is.null(x_y_decimals) | !is.null(x_y_scientific)) {
    if(is.null(x_y_decimals)){
      x_y_decimals <- c(0,0)
    }
    if(is.null(x_y_scientific)){
      x_y_scientific <- c(FALSE, FALSE)
    }

    aplot <- aplot + scale_y_continuous(
      limits = y_limits,
      breaks = y_major_breaks,
      minor_breaks = y_minor_breaks,
      labels = fmt_decimals(x_y_decimals[[2]], x_y_scientific[[2]])
    )
  }else {
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

  # -----------------add symmetry reference line----------------
  aplot <- aplot + ggplot2::geom_abline(
    linewidth = line_width,
    color = line_color,
    alpha = line_alpha,
    linetype = line_linetype
  )

  return(aplot)
}
