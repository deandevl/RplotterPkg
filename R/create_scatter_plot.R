#' @title create_scatter_plot
#'
#' @description Function is a wrapper around ggplot2 geom_point and geom_line to produce a scatter plot.
#'  Function returns a ggplot2 plot object of x/y scatter points/lines. Options are provided for axis scaling and
#'  variable/non-variable dependent aesthetics.
#'
#' @param df The target data frame from which the scatter points are plotted.
#' @param aes_x The x axis variable name from 'df'. This is a required discrete factor, continuous numeric,
#'  or Date/POSIXct variable.
#' @param aes_y The y axis variable name from 'df'. This is a required discrete factor or continuous numeric.
#' @param aes_color The variable name from df' for the variable dependent aesthetic mapping for color.
#' @param aes_fill The variable name from 'df' for the variable dependent aesthetic mapping for fill.
#' @param aes_size The variable name from 'df' for the variable dependent aesthetic mapping for size.
#' @param aes_alpha The variable name from 'df' for the variable dependent aesthetic mapping for alpha.
#' @param aes_linetype The variable name from 'df' for the variable dependent aesthetic mapping for linetype.
#' @param aes_label The variable name from 'df' for the variable dependent aesthetic mapping for labeling.
#' @param aes_label_color A string that sets the color of labels.
#' @param aes_label_size A numeric that sets the size of labels.
#' @param aes_label_nudge_x A numeric that nudges the label's horizontal position.
#' @param aes_label_nudge_y A numeric that nudges the label's vertical position.
#' @param aes_CI_lwr Sets the column from 'df' for the lower confidence interval with reference to the x or y axis.
#' @param aes_CI_upr Sets the column from 'df' for the upper confidence interval with reference to the x or y axis.
#' @param position A string or function that does a slight adjustment to overlapping points.  Typical values are
#'  "jitter" or \code{position_jitter(width = 0.1, height = 0.1)}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the 'title' and 'subtitle'.
#' @param x_title A string that sets the x axis title. If \code{NULL} (the default) then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If\code{NULL} (the default) then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Depending on the class of 'aes_x', a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of 'aes_x', a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param x_minor_breaks Depending on the class of 'aes_x', a numeric/Date/POSIXct vector or function that defines
#'  the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as 'x_major_breaks', that labels the major tics.
#' @param x_major_date_breaks If the class of 'aes_x' is Date/POSIXct, a string containing the number and date unit
#'  for major breaks. Examples: "1 year", "4 sec", "3 month", "2 week".
#' @param x_minor_date_breaks If the class of 'aes_x' is Date/POSIXct, a string containing the number and date unit
#'  for minor breaks.
#' @param x_date_labels If the class of 'aes_x' is Date/POSIXct, a string containing the format codes, the strftime
#'  format, for the date.
#'  Examples: \code{\%Y-\%m}, \code{\%Y/\%b/\%d}, \code{\%H-\%M-\%S}
#' @param x_log10 A logical which if \code{TRUE} will use a log10 scale for the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as 'y_major_breaks', that labels the major tics.
#' @param y_log10 A logical which if \code{TRUE} will use a log10 scale for the y axis.
#' @param x_y_decimals A two element numeric vector that set the number of decimals for the x and y tic labels.
#' @param x_y_scientific A two element logical vector that if TRUE uses scientific notation for the x and y tic labels.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param show_pts A logical which if \code{FALSE} will plot only the lines if 'connect' is \code{TRUE}.
#' @param pts_color A string that sets the color attribute of the points.
#' @param pts_fill A string that sets the fill color attribute of the points.
#' @param pts_shape A numeric integer that sets the shape attribute of the points. Typical values are 21 "circle",
#'  22 "square", 23 "diamond", 24 "up triangle", 25 "down triangle".
#' @param pts_stroke A numeric that sets the drawing stroke  width attribute for a point shape.
#' @param pts_size A numeric value that sets the size attribute of the points.
#' @param pts_line_alpha A numeric value that sets the alpha level attribute of points and connected lines.
#' @param connect A logical which if \code{TRUE} then points will be connected with a line.
#' @param line_width A numeric value that sets the width of lines if 'connect' is TRUE.
#' @param line_color A string that sets the color of the lines if 'connect' is TRUE.
#' @param connect_linetype A string that sets line type "twodash", "solid", "longdash", "dotted", "dotdash",
#'  "dashed", "blank" if 'connect' is \code{TRUE}.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
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
#' @param bold_y A numeric that sets the y-intercept for plotting a bold horizontal line.
#' @param bold_y_color A string that sets the color of 'bold_y'.
#' @param bold_y_linetype A string that set the linetype of 'bold_y'.
#' @param CI_dir A string that sets the axis orientation of the confidence intervals. Acceptable values are "x" or "y".
#' @param CI_show_line A logical that if \code{TRUE} shows lower/upper confidence lines.
#' @param CI_show_errorbar A logical that if \code{TRUE} shows error bars between 'aes_y' and lower/upper confidence lines.
#' @param CI_show_ribbon A logical that if \code{TRUE} shows a filled ribbon between the lower/upper confidence lines.
#' @param CI_line_color A string that sets the lower/upper confidence line colors.
#' @param CI_line_width A numeric that sets the lower/upper confidence line widths.
#' @param CI_linetype A string that sets the linetype for lower/upper confidence lines.
#' @param CI_errorbar_color A string that sets the error bars' color.
#' @param CI_errorbar_width A numeric that sets the error bars' width.
#' @param CI_ribbon_color A string that sets the fill color for the confidence interval ribbon.
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
#' RplotterPkg::create_scatter_plot(
#'   df = ggplot2::economics,
#'   aes_x = "date",
#'   aes_y = "unemploy",
#'   pts_shape = 21,
#'   pts_fill = "black",
#'   line_color = "violet",
#'   connect = TRUE,
#'   title = "US Monthly Unemployment",
#'   subtitle = "July, 1967 to April, 2015 (in thousands)",
#'   x_title = "Date",
#'   y_title = "Unemployment",
#'   rot_y_tic_label = TRUE,
#'   x_date_labels = "%Y-%b",
#'   x_major_date_breaks = "5 year",
#'   y_limits = c(0, 16000),
#'   y_major_breaks = seq(0, 16000, 2000),
#'   show_minor_grids = FALSE,
#'   bold_y = 8000,
#'   bold_y_color = "red",
#'   bold_y_linetype = "dashed"
#' )
#'
#' @import ggplot2
#' @importFrom methods is
#'
#' @export
create_scatter_plot <- function(
    df = NULL,
    aes_x = NULL,
    aes_y = NULL,
    aes_color = NULL,
    aes_fill = NULL,
    aes_size = NULL,
    aes_alpha = NULL,
    aes_linetype = NULL,
    aes_label = NULL,
    aes_label_color = "black",
    aes_label_size = 6,
    aes_label_nudge_x = 0.0,
    aes_label_nudge_y = -0.3,
    aes_CI_lwr = NULL,
    aes_CI_upr = NULL,
    position = position_jitter(width = 0.0, height = 0.0),
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
    show_pts = TRUE,
    pts_fill = "white",
    pts_shape = 21,
    pts_stroke = 1.0,
    pts_color = "black",
    pts_size = 1.0,
    pts_line_alpha = 1.0,
    connect = FALSE,
    line_width = 0.6,
    line_color = "black",
    connect_linetype = "solid",
    show_major_grids = TRUE,
    show_minor_grids = TRUE,
    show_legend = TRUE,
    legend_pos = "right",
    legend_key_width = 0.7,
    legend_key_height = 0.7,
    legend_key_backgrd = "white",
    panel_color = "white",
    panel_border_color = "black",
    bold_y = NULL,
    bold_y_color = "black",
    bold_y_linetype = "dashed",
    CI_dir = "y",
    CI_show_line = FALSE,
    CI_show_errorbar = FALSE,
    CI_show_ribbon = FALSE,
    CI_line_color = "black",
    CI_line_width = 1.0,
    CI_linetype = "dashed",
    CI_errorbar_color = "black",
    CI_errorbar_width = 1.0,
    CI_ribbon_color = "gray70",
    silent_NA_warning = FALSE,
    png_file_path = NULL,
    png_width_height = c(480,480)) {

  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
  }
  if(is.null(aes_x) | is.null(aes_y)){
    stop("Both aes_x and aes_y are required arguments.")
  }

  if(CI_show_line | CI_show_errorbar | CI_show_ribbon){
    if(is.null(aes_CI_lwr) | is.null(aes_CI_upr)){
      stop("Both aes_CI_lwr and aes_CI_upr are required arguments for confidence limits.")
    }
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
  if(!is.null(aes_alpha)){
    aes_alpha <- rlang::sym(aes_alpha)
  }
  if(!is.null(aes_linetype)){
    aes_linetype <- rlang::sym(aes_linetype)
  }

  # -----------------Set geom attribute default values---------------
  ggplot2::update_geom_defaults(
    "point",
    list(
      size = pts_size,
      fill = pts_fill,
      shape = pts_shape,
      stroke = pts_stroke,
      color = pts_color,
      alpha = pts_line_alpha
    )
  )

  ggplot2::update_geom_defaults(
    "line",
    list(
      linewidth = line_width,
      color = line_color,
      linetype = connect_linetype,
      alpha = pts_line_alpha
    )
  )

  # -------------------Define the main ggplot2 plot object/geoms-----------
  aplot <- ggplot(
    data = df,
    aes(
      x = !!sym(aes_x),
      y = !!sym(aes_y),
    )
  )

# -----------------If connect lines, are we showing points?---------
  if(connect){
    if(show_pts){
      aplot <- aplot +
        geom_point(data = df,
          aes(
            color = !!aes_color,
            fill = !!aes_fill,
            size = !!aes_size,
            alpha = !!aes_alpha
          ),
          position = position,
          na.rm = silent_NA_warning
        )
    }
  }else {
    aplot <- aplot +
      geom_point(data = df,
        aes(
          color = !!aes_color,
          fill = !!aes_fill,
          size = !!aes_size,
          alpha = !!aes_alpha
        ),
        position = position,
        na.rm = silent_NA_warning
      )
  }
# -------------Are we connecting the points----------
  if(connect){
    aplot <- aplot +
      geom_line(
        aes(
          linetype = !!aes_linetype,
          color = !!aes_color,
          alpha = !!aes_alpha
        ),
        na.rm = silent_NA_warning
      )
  }

  # -----------------Are we labeling the points?----------------
  if(!is.null(aes_label)){
    aplot <- aplot +
      ggplot2::geom_text(
        aes(
          label = !!sym(aes_label),
          hjust = aes_label_nudge_x,
          vjust = aes_label_nudge_y
        ),
        color = aes_label_color,
        size = aes_label_size,
        na.rm = TRUE
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
      axis.title.x = element_text(size = axis_text_size + 4, color = "black"),
      axis.text.y = element_text(size = axis_text_size, color = "black", angle = rot_y_tic_angle),
      axis.title.y = element_text(size = axis_text_size + 4, color = "black")
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

  # ---------------x axis tic scaling-------------------
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

  # ---------------y axis tic scaling-------------------
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

  # -------------add bold horizontal line if requested------------
  if(!is.null(bold_y)) {
    aplot <- aplot +
      geom_hline(data = df, aes(yintercept = bold_y), lwd = 1.2, linetype = bold_y_linetype, color = bold_y_color)
  }

  # ------------------draw confidence intervals if requested------------
  if(CI_show_line | CI_show_errorbar | CI_show_ribbon){
    if(CI_show_errorbar){
      if(CI_dir == "x"){
        aplot <- aplot +
          geom_errorbar(data = df, aes(xmin = !!sym(aes_CI_lwr), xmax = !!sym(aes_CI_upr)), color = CI_errorbar_color, width = CI_errorbar_width)
      }else if(CI_dir == "y"){
        aplot <- aplot +
          geom_errorbar(data = df, aes(ymin = !!sym(aes_CI_lwr), ymax = !!sym(aes_CI_upr)), color = CI_errorbar_color, width = CI_errorbar_width)
      }
    }
    if(CI_show_ribbon){
      if(CI_dir == "x"){
        aplot <- aplot +
          geom_ribbon(data = df, aes(xmin = !!sym(aes_CI_lwr), xmax = !!sym(aes_CI_upr)), fill = CI_ribbon_color, alpha = 0.4)
      }else if(CI_dir == "y"){
        aplot <- aplot +
          geom_ribbon(data = df, aes(ymin = !!sym(aes_CI_lwr), ymax = !!sym(aes_CI_upr)), fill = CI_ribbon_color, alpha = 0.4)
      }
    }
    if(CI_show_line) {
      aplot <- aplot +
        geom_line(data = df, aes(x = !!sym(aes_x), y = !!sym(aes_CI_lwr)), color = CI_line_color, lwd = CI_line_width, linetype = CI_linetype) +
        geom_line(data = df, aes(x = !!sym(aes_x), y = !!sym(aes_CI_upr)), color = CI_line_color, lwd = CI_line_width, linetype = CI_linetype)
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
