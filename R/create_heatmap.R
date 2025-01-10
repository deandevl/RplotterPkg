#' @title create_heatmap
#'
#' @description Function returns a ggplot2 x/y plot of a pair of discrete variables. Instead of a point, a
#'   rectangle/cell/tile is located for each x/y value with the cell itself being colored or sized depending on the value
#'   of a third associated discrete or continuous variable. Function wraps ggplot2 geom_tile to produce a heatmap that shows
#'   magnitude as an array of cells in two dimensions.
#'
#' @param df The target data frame from which the heatmap is plotted.
#' @param aes_x The x axis variable name from \code{df}. This is a required discrete numeric,
#'  Date/POSIXct variable.
#' @param aes_y The y axis variable name from \code{df}. This is a required discrete numeric.
#' @param aes_label Sets the variable name from \code{df} as the source for the tile's text.
#' @param aes_color The discrete/continuous variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill The discrete/continuous variable name from \code{df} for the aesthetic mapping for fill.
#' @param aes_size The variable name from \code{df} for the aesthetic mapping for size.
#' @param aes_width The variable name from \code{df} for the aesthetic mapping for width.
#' @param aes_height The variable name from \code{df} for the aesthetic mapping for height.
#' @param tile_sz A numeric that sets the tile's size width attribute in millimeters.
#' @param tile_color A numeric that sets the tile's attribute border color.
#' @param tile_fill A string that sets the fill color attribute for the tile.
#' @param label_sz A numeric that sets the size of the label.
#' @param label_color A string that sets the label's color.
#' @param label_fontface A string that sets the label's font face. Acceptable values are "plain", "bold",
#'  "italic", "bold.italic".
#' @param label_alpha A numeric that sets the label's alpha value.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title. If NULL (the default)  then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If NULL (the default)  then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
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
#' @param x_y_decimals A two element numeric vector that set the number of decimals for the x and y tic labels.
#' @param x_y_scientific A two element logical vector that if TRUE uses scientific notation for the x and y tic labels.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param panel_color A string in hexidecimal or color name that sets the plot panel's color.
#'   The default is "white".
#' @param panel_border_color A string in hexidecimal or color name that sets the plot panel's border color.
#'   The default is "black".
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  "top", "bottom", "left", "right".
#' @param legend_key_width A numeric that sets the legend width in cm.
#' @param legend_key_height A numeric that sets the legend height in cm.
#' @param legend_key_backgrd A string that sets the legend's background color.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#'  are removed.
#' @param png_file_path A character string with the directory and file name to produce
#'  a png image of the plot.
#' @param png_width_height A numeric vector that sets the width and height of the png image in pixels. The
#'  default is c(480,480).  There are 37.8 pixels in a centimeter.
#'
#' @return A ggplot class object.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' library(methods)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_heatmap(
#'   df = RplotterPkg::spinrates,
#'   aes_x = "velocity",
#'   aes_y = "spinrate",
#'   aes_fill = "swing_miss",
#'   aes_label = "swing_miss",
#'   label_fontface = "bold",
#'   title = "Likelihood of swinging and missing on a fastball",
#'   x_title = "Velocity",
#'   y_title = "Spinrate",
#'   rot_y_tic_label = TRUE,
#') +
#'  ggplot2::scale_fill_gradientn(
#'    colors = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
#'    n.breaks = 8
#'  ) +
#'  ggplot2::guides(
#'    fill = ggplot2::guide_colorbar(
#'      ticks.colour = "black"
#'    )
#'  )
#'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym
#' @importFrom methods is
#' @import ggplot2
#'
#' @export
create_heatmap <- function(
    df,
    aes_x = NULL,
    aes_y = NULL,
    aes_label = NULL,
    aes_color = NULL,
    aes_fill = NULL,
    aes_size = NULL,
    aes_width = NULL,
    aes_height = NULL,
    tile_sz = 1.1,
    tile_color = "white",
    tile_fill = "white",
    label_sz = 6,
    label_color = "black",
    label_fontface = "plain",
    label_alpha = 1.0,
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
    x_major_date_breaks = waiver(),
    x_minor_date_breaks = waiver(),
    x_date_labels = waiver(),
    x_log10 = FALSE,
    y_limits = NULL,
    y_major_breaks = waiver(),
    y_minor_breaks = waiver(),
    y_labels = waiver(),
    y_log10 = FALSE,
    x_y_decimals = NULL,
    x_y_scientific = NULL,
    axis_text_size = 11,
    panel_color = "white",
    panel_border_color = "black",
    show_legend = TRUE,
    legend_pos = "right",
    legend_key_width = 0.5,
    legend_key_height = 0.7,
    legend_key_backgrd = "white",
    silent_NA_warning = FALSE,
    png_file_path = NULL,
    png_width_height = c(480,480)){

  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
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
  if(!is.null(aes_size)){
    aes_size <- rlang::sym(aes_size)
  }
  if(!is.null(aes_width)){
    aes_width <- rlang::sym(aes_width)
  }
  if(!is.null(aes_height)){
    aes_height <- rlang::sym(aes_height)
  }
  if(!is.null(aes_label)){
    aes_label <- rlang::sym(aes_label)
  }

  # -------------Set geom attribute default values------------------
  ggplot2::update_geom_defaults(
    "tile",
    list(
      size = tile_sz,
      color = tile_color,
      fill = tile_fill
    )
  )

  ggplot2::update_geom_defaults(
    "text",
    list(
      color = "black"
    )
  )
  # ------------------Define the main ggplot2 plot object/geoms------------
  aplot <- ggplot2::ggplot(
    data = df,
    aes(
      x = !!sym(aes_x),
      y = !!sym(aes_y)
    )
  ) +
  ggplot2::geom_tile(
    aes(
      color = !!aes_color,
      fill = !!aes_fill,
      size = !!aes_size,
      width = !!aes_width,
      height = !!aes_height
    ),
    na.rm = silent_NA_warning
  ) +
  ggplot2::geom_text(
    aes(
      label = !!aes_label,
    ),
    size = label_sz,
    color = label_color,
    fontface = label_fontface,
    alpha = label_alpha
  )

  # -------------------Additional ggplot2 components------------------------
  # ----------------------title, subtitle, and caption-----------------
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

  # --------------------panel---------------------
  aplot <- aplot +
    theme(
      panel.background = element_rect(fill = panel_color, color = panel_border_color, linewidth = 2)
    )
  # -------------------rotate tic labels--------------------
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

  # -------------------axis tic scales------------------
  # define function for formatting decimals
  fmt_decimals <- function(decimals=0, sci=FALSE){
    function(x) {format(x,nsmall = decimals,scientific=sci)}
  }

  # ---------------x axis tic scale-------------------
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
    }
  }

  # ---------------y axis tic scales-------------------
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

  # -----------------save the plot as a png file?------------------
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
