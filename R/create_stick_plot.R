#' @title create_stick_plot
#'
#' @description Function wraps a ggplot2 geom_linerange to produce a "stick" plot.
#'   Function returns a plot object showing vertical/horizontal lines that run from a base value to
#'   a measurement value. Options are provided for scaling.
#'
#' @param df The required target data frame from which the "stick" lines are drawn.
#' @param base_val A numeric that sets the base value from which the "stick" originates.
#'  The default value is 0.
#' @param aes_x Sets the x axis variable name from \code{df}.  Can be a numeric/Date/POSIXct variable and is required.
#' @param aes_y Sets the y axis variable name from \code{df} and controls the height of
#'  individual "sticks". The argument is required
#' @param aes_color Sets the variable name from \code{df} for the aesthetic mapping for color.
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
#' @param x_major_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that
#'  defines the exact major tic locations along the x axis.
#' @param x_minor_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines
#'  the exact minor tic locations along the x axis.
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
#' @param line_color A string that sets the color attribute of the lines.
#' @param line_width A numeric value that sets the width of lines.
#' @param line_type A string that sets the linetype. The default is "solid".
#' @param line_alpha A numeric value that sets the degree of color alpha attribute for the lines.
#' @param show_major_grids A logical that controls the appearence of major grids.
#' @param show_minor_grids A logical that controls the appearence of minor grids.
#' @param panel_color A string in hexidecimal or color name that sets the plot panel's color.
#'   The default is "white".
#' @param panel_border_color A string in hexidecimal or color name that sets the plot panel's border color.
#'   The default is "black".
#' @param show_legend A logical that controls the appearence of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  "top", "bottom", "left", "right".
#' @param legend_key_width A numeric that sets the legend width in cm.
#' @param legend_key_height A numeric that sets the legend height in cm.
#' @param legend_key_backgrd A string that sets the legend's background color.
#' @param bold_y A numeric that plots a bold horizontal line at this y value.
#' @param bold_y_color A string that sets the bold horizontal line color. Default is "black".
#' @param silent_NA_warning A logical that controls the appearance of a console warning when NA's
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
#' RplotterPkg::create_stick_plot(
#'   df = RplotterPkg::air_passengers,
#'   aes_x = "time",
#'   aes_y = "value",
#'   title = "Monthly Totals of International Airline Passengers",
#'   subtitle = "1949 - 1960 (classic Box & Jenkins)",
#'   x_title = "Time",
#'   y_title = "Totals",
#'   x_major_date_breaks = "1 year",
#'   x_date_labels = "%Y",
#'   rot_y_tic_label = TRUE,
#'   show_minor_grids = FALSE,
#'   bold_y = 0.0
#' )
#'
#' @importFrom rlang sym
#' @importFrom methods is
#' @import ggplot2
#'
#' @export
create_stick_plot <- function(
  df = NULL,
  base_val = 0,
  aes_x = NULL,
  aes_y = NULL,
  aes_color = NULL,
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
  x_date_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  line_color = "black",
  line_width = 0.8,
  line_type = "solid",
  line_alpha = 1.0,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  panel_color = "white",
  panel_border_color = "black",
  show_legend = TRUE,
  legend_pos = "right",
  legend_key_width = 1.5,
  legend_key_height = 1.5,
  legend_key_backgrd = "white",
  bold_y = NULL,
  bold_y_color = "black",
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

  if(!is.null(aes_color)){
    aes_color <- rlang::sym(aes_color)
  }

  # -----------------Set geom attribute default values---------------
  ggplot2::update_geom_defaults(
    "linerange",
    list(
      color = line_color,
      linewidth = line_width,
      alpha = line_alpha,
      linetype = line_type
    )
  )

  # -------------------Define the main ggplot2 plot object/geoms-----------
  aplot <- ggplot(data = df) +
    geom_linerange(
      aes(
        x = !!sym(aes_x),
        ymin = base_val,
        ymax = !!sym(aes_y),
        color = !!aes_color
      ),
      na.rm = silent_NA_warning
    )
  # -------------------Additional ggplot2 components------------------------

  # ---------------------------Add a horizontal bold line?-------
  if(!is.null(bold_y)) {
    aplot <- aplot +
      geom_hline(aes(yintercept = bold_y), lwd = 1.2, linetype = "solid", color = bold_y_color)
  }

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

  # -------------------x axis tic scaling------------------
  if(is(df[[aes_x]], "Date")) {
    aplot <- aplot + scale_x_date(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = x_labels,
      date_breaks = x_major_date_breaks,
      date_labels = x_date_labels
    )
  }else if(is(df[[aes_x]],"POSIXct") || is(df[[aes_x]],"POSIXlt")) {
    aplot <- aplot + scale_x_datetime(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = x_labels,
      date_breaks = x_major_date_breaks,
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

  # -------------------y axis tic scaling------------------
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


