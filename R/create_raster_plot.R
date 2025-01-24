#' @title create_raster_plot
#'
#' @description The function is wrapper around \code{ggplot2::geom_raster()} that plots from a data.frame with
#'  columns for x and y coordinates along with an associated numeric attribute.
#'
#' The function accepts a data frame with columns for numeric x/y values and an attribute
#'  that will be mapped as a color or fill aesthetic. Function provides additional
#'  ggplot2 text labeling and axis scaling.
#'
#' @param df The required data.frame derived from a raster file (such as *.tiff) to be plotted.
#' @param aes_x The required name of aesthetic variable from 'df' for the x dimension.
#' @param aes_y The required name of aesthetic variable from 'df' for the y dimension.
#' @param aes_color The variable name from 'df' for the attribute dependent aesthetic mapping for color.
#' @param aes_fill The variable name from 'df' for the attribute dependent aesthetic mapping for fill.
#' @param interpolate A logical which if TRUE interpolate linearly.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the 'title' and 'subtitle'.
#' @param x_title A string that sets the x axis title. If NULL (the default) then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If NULL (the default)  then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param panel_color A string in hexidecimal or color name that sets the plot panel's color.
#'   The default is "white".
#' @param panel_border_color A string in hexidecimal or color name that sets the plot panel's border color.
#'   The default is "black".
#' @param x_limits Depending on the class of 'aes_x', a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use \code{NA} to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of 'aes_x', a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param x_minor_breaks Depending on the class of 'aes_x', a numeric/Date/POSIXct vector or function that defines
#'  the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as 'x_major_breaks', that labels the major tics.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as 'y_major_breaks', that labels the major tics.
#' @param scale_breaks A string/numeric vector that defines the scale breaks.
#' @param scale_values A string/numeric vector that defines the possible values.
#' @param scale_limits A string/numeric vector that defines the scale limits.
#' @param scale_labels An optional string vector that defines the scale labels. Vector must be the same length
#' as \code{scale_breaks}.
#' @param scale_colors Vector of colors to use for n-color gradient.
#' @param scale_na_value A string that sets the color for missing values.
#' @param own_scale A logical which if \code{TRUE}, then your own scaling may be appended to the plot without using the above
#'   scale_* parameters.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  "top", "bottom", "left", "right".
#' @param legend_key_width A numeric that sets the legend width in cm.
#' @param legend_key_height A numeric that sets the legend height in cm.
#' @param legend_key_backgrd A string that sets the legend's background color.
#'
#' @return A ggplot class object.
#'
#' @examples
#' library(ggplot2)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_raster_plot(
#'   df = RplotterPkg::kentucky_elevation,
#'   title = "County Elevations in Southeast Kentucky",
#'   aes_x = "x",
#'   aes_y = "y",
#'   aes_fill = "elevation"
#' ) +
#' ggplot2::geom_sf(
#'   data = RplotterPkg::kentucky_counties,
#'   aes(x = NULL, y = NULL),
#'   alpha = 0,
#'   linewidth = 1.5
#' ) +
#' ggplot2::geom_sf_text(
#'   data = RplotterPkg::kentucky_counties,
#'   aes(x = NULL, y = NULL, label = ID)
#' )
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_raster
#' @importFrom rlang sym
#' @importFrom grDevices heat.colors
#'
#' @export
create_raster_plot <- function(
  df,
  aes_x = NULL,
  aes_y = NULL,
  aes_color = NULL,
  aes_fill = NULL,
  interpolate = FALSE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  center_titles = FALSE,
  x_title = NULL,
  y_title = NULL,
  hide_x_tics = FALSE,
  hide_y_tics = FALSE,
  panel_color = "white",
  panel_border_color = "black",
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_minor_breaks = waiver(),
  x_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  scale_breaks = waiver(),
  scale_values = NULL,
  scale_limits = NULL,
  scale_labels = waiver(),
  scale_colors = heat.colors(8),
  scale_na_value = "gray50",
  own_scale = FALSE,
  show_legend = TRUE,
  legend_pos = "right",
  legend_key_width = 0.5,
  legend_key_height = 0.7,
  legend_key_backgrd = "white"
){

  if(is.null(df)){
    stop("The data frame parameter(df) must be defined.")
  }

  if(is.null(aes_x) | is.null(aes_y)){
    stop("Both aes_x and aes_y are required arguments.")
  }

  if(!is.null(aes_fill)){
    aes_fill <- rlang::sym(aes_fill)
  }
  if(!is.null(aes_color)){
    aes_color <- rlang::sym(aes_color)
  }

  # -------------------Define the main ggplot2 plot object/geoms-----------
  aplot <- ggplot2::ggplot(
    data = df,
    aes(
      x = !!sym(aes_x),
      y = !!sym(aes_y)
    )
  ) +
  ggplot2::geom_raster(
    aes(
      color = !!aes_color,
      fill = !!aes_fill
    ),
    interpolate = interpolate
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

  # -------------------hide axis tics & tic labels?------------------
  # x axis
  if(hide_x_tics){
      aplot <- aplot +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
  }else{
    aplot <- aplot + scale_x_continuous(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = x_labels
    )
  }
  # y axis
  if(hide_y_tics){
      aplot <- aplot +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
  }

  #-------------y axis scaling--------------
  aplot <- aplot + scale_y_continuous(
    limits = y_limits,
    breaks = y_major_breaks,
    minor_breaks = y_minor_breaks,
    labels = y_labels
  )

  # -------------------scaling related parameters--------------------
  if(!own_scale){
    if(!is.null(aes_fill)) {
      if(is.factor(df[[aes_fill]])){
        aplot <- aplot +
          ggplot2::scale_fill_manual(
            breaks = scale_breaks,
            values = scale_values,
            limits = scale_limits,
            labels = scale_labels,
            na.value = scale_na_value
          )
      }else{
        aplot <- aplot +
          ggplot2::scale_fill_gradientn(
            breaks = scale_breaks,
            limits = scale_limits,
            labels = scale_labels,
            colors = scale_colors,
            values = scale_values,
            na.value = scale_na_value
          )
      }
    }else if(!is.null(aes_color)){
      if(is.factor(df[[aes_color]])){
        aplot <- aplot +
          ggplot2::scale_color_manual(
            breaks = scale_breaks,
            values = scale_values,
            limits = scale_limits,
            labels = scale_labels,
            na.value = scale_na_value
          )
      }else{
        aplot <- aplot +
          ggplot2::scale_color_gradientn(
            breaks = scale_breaks,
            limits = scale_limits,
            labels = scale_labels,
            colors = scale_colors,
            values = scale_values,
            na.value = scale_na_value
          )
      }
    }
  }

  # -------------------legend related parameters---------------------------
  if(!show_legend){
    aplot <- aplot +
      theme(legend.position = "none")
  }else {
    aplot <- aplot +
      theme(
        legend.position = legend_pos,
        legend.key = element_rect(fill = legend_key_backgrd),
        legend.key.width = unit(legend_key_width, "cm"),
        legend.key.height = unit(legend_key_height, "cm")
      )
  }
  return(aplot)
}
