#' Function produces a heatmap that shows magnitude as an array of cells in two dimensions.
#'
#' @description Function returns a ggplot2 x/y plot of a pair of discrete variables. Instead of a point, a
#' rectangle/cell/tile is located for each x/y value with the cell itself being colored or sized depending on the value
#' of a third associated discrete or continuous variable.
#'
#' @param df The target data frame from which the heatmap is plotted.
#' @param aes_x A string that sets the x axis variable name from \code{df}. Is a required discrete numeric,
#'  Date/POSIXct variable.
#' @param aes_y A string that sets the y axis variable name from \code{df}. Is a required discrete numeric.
#' @param aes_label A string that sets the variable name from \code{df} as the required source for the tile's text.
#' @param aes_fill A string that sets the discrete/continuous variable name from \code{df} for the aesthetic mapping for fill.
#' @param aes_size A string that sets the variable name from \code{df} for the aesthetic mapping for size.
#' @param aes_width A string that sets the variable name from \code{df} for the aesthetic mapping for width.
#' @param aes_height A string that sets the variable name from \code{df} for the aesthetic mapping for height.
#' @param tile_sz A numeric that sets the tile's size width in millimeters.
#' @param tile_color A numeric that sets the tile's border color.
#' @param tile_fill A string that sets the fill color for the tile.
#' @param tile_square A logical which if TRUE makes the tiles square instead of rectangular.
#' @param label_sz A numeric that sets the size of the label.
#' @param label_color A string that sets the label's color.
#' @param label_fontface A string that sets the label's font face. Acceptable values are \dQuote{plain}, \dQuote{bold},
#'  \dQuote{italic}, \dQuote{bold.italic}.
#' @param label_alpha A numeric that sets the label's alpha value.
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
#' @param x_labels A character vector with the same length as \code{x_major_breaks}, that labels the major tics.
#' @param x_major_date_breaks If the class of \code{aes_x} is Date/POSIXct, a string containing the number and date unit
#'  for major breaks. \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels If the class of \code{aes_x} is Date/POSIXct, a string containing the format codes, the strftime
#'  format, for the date.
#'  Examples: \code{\%Y-\%m}, \code{\%Y/\%b/\%d}, \code{\%H-\%M-\%S}
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#'  are removed.
#'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @return A plot object
#'
#' @author Rick Dean
#'
#' @export
create_heatmap <- function(
  df,
  aes_x,
  aes_y,
  aes_label,
  aes_fill = NULL,
  aes_size = NULL,
  aes_width = NULL,
  aes_height = NULL,
  tile_sz = 1.1,
  tile_color = "white",
  tile_fill = "white",
  tile_square = FALSE,
  label_sz = 11,
  label_color = "white",
  label_fontface = "plain",
  label_alpha = 1.0,
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
  y_labels = waiver(),
  axis_text_size = 11,
  show_legend = TRUE,
  legend_pos = "top",
  silent_NA_warning = FALSE) {

  df_ref <- data.table::setDT(df)

  aplot <- ggplot(data = df_ref, aes(x = !!rlang::sym(aes_x), y = !!rlang::sym(aes_y)))
  if(!is.null(aes_fill)){
    aplot <- aplot +
      geom_tile(aes(fill = !!sym(aes_fill)),
        size = tile_sz,
        color = tile_color,
        show.legend = show_legend,
        na.rm = silent_NA_warning
      )
  }else if(!is.null(aes_size)){
    aplot <- aplot +
      geom_tile(aes(size = !!sym(aes_size)),
        color = tile_color,
        fill = tile_fill,
        show.legend = show_legend,
        na.rm = silent_NA_warning
      )
  }else if(!is.null(aes_width)){
    aplot <- aplot +
      geom_tile(aes(width = !!sym(aes_width)),
        color = tile_color,
        fill = tile_fill,
        show.legend = show_legend,
        na.rm = silent_NA_warning
      )
  }else if(!is.null(aes_height)){
    aplot <- aplot +
      geom_tile(aes(height = !!sym(aes_height)),
        color = tile_color,
        fill = tile_fill,
        show.legend = show_legend,
        na.rm = silent_NA_warning
      )
  }else {
    aplot <- aplot +
      geom_tile(
        color = tile_color,
        fill = tile_fill,
        size = tile_sz,
        na.rm = silent_NA_warning
      )
  }

  if(tile_square){
    aplot <- aplot + coord_fixed()
  }

  aplot <- aplot +
    geom_text(aes(label = !!sym(aes_label)), color = label_color, fontface = label_fontface, alpha = label_alpha)

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

  aplot <- aplot + labs(title = title, subtitle = subtitle, x = x_title, y = y_title)

  return(aplot)
}
