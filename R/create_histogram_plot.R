#' Function creates a histogram.
#'
#' @description Function plots a ggplot2 based histogram with options for scaling and viewing observation locations.
#'
#' @param df The target data frame for the bar chart.
#' @param aes_x A string that sets the x axis continuous variable name from \code{df}.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill A string that sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param position A string that sets the bar positions.  Acceptable values are \code{dodge, dodge2}(side by side), \code{identity}(overlap) or \code{stack}.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param binwidth A numeric that sets the bin width of the histogram in units of \code{aes_x}.  If \code{aes_x} is a
#'  date variable then \code{binwidth} is the number of days and if a time variable then \code{binwidth} is
#'  the number of seconds.
#' @param bins An integer that sets the number of bins for the histogram. Is overridden by \code{binwidth}. Default is 20.
#' @param bin_breaks A numeric vector giving the bin boundaries. Overrides \code{binwidth} and \code{bins}
#' @param bar_fill A string that sets the fill color for the bars.
#' @param bar_color A string that sets the outline color for the bars.
#' @param bar_alpha A numeric that set the alpha component to \code{bar_color}.
#' @param bar_size A numeric that sets the outline thickness of the bars.
#' @param palette_colors A character vector to set the palette colors.
#' @param x_limits A numeric 2 element vector or function that sets the minimum and maximum for the x axis.  Use NA to
#'  refer to the existing minimum and maximum.
#' @param x_major_breaks A numeric vector or function that sets the major tic locations along the x axis.
#' @param x_minor_breaks A numeric vector or function that sets the minor tic locations along the x axis.
#' @param x_labels A character vector or function giving x axis tic labels.  Must be the same length as \code{x_breaks}.
#' @param y_limits A numeric 2 element vector or function that sets the minimum and maximum for the y axis.  Use NA to
#'  refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that sets the major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that sets the minor tic locations along the y axis.
#' @param y_labels A character vector or function giving y axis tic labels.  Must be the same length as \code{y_breaks}.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param do_coord_flip A logical which if TRUE will flip the x and y axis'.
#' @param bar_labels A logical which if \code{TRUE} will label each bar with its value.
#' @param bar_label_size A numeric that sets the size of the bar labels
#' @param bar_label_color A string that sets the color of the bar labels
#' @param plot_obs A logical which if \code{TRUE} plots a line for each observation along the axis margin.
#' @param plot_obs_len A numeric that sets the length of the \code{plot_obs} lines.
#' @param plot_obs_color A string that sets the color of the \code{plot_obs} lines.
#' @param plot_obs_jitter A logical which if \code{TRUE} will add a slight horizontal adjustment to overlapping observations.
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
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
#' @return A plot object.
#'
#' @author Rick Dean
#'
#' @export
create_histogram_plot <- function(
  df,
  aes_x,
  aes_color = NULL,
  aes_fill = NULL,
  position = "stack",
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_title = aes_x,
  y_title = "Count",
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  bins = 20,
  binwidth = NULL,
  bin_breaks = NULL,
  bar_fill = NA,
  bar_color = "black",
  bar_alpha = 0.4,
  bar_size = 1.0,
  palette_colors = NULL,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_minor_breaks = waiver(),
  x_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  do_coord_flip = FALSE,
  bar_labels = FALSE,
  bar_label_size = 6,
  bar_label_color = "black",
  plot_obs = FALSE,
  plot_obs_len = 0.02,
  plot_obs_color = "black",
  plot_obs_jitter = FALSE,
  do_x_title = TRUE,
  do_y_title = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_legend = TRUE,
  legend_pos = "top",
  silent_NA_warning = FALSE) {

  df_copy <- data.table::as.data.table(df)

  aplot <- ggplot(data = df_copy, aes(x = !!rlang::sym(aes_x)))

  if(!is.null(aes_color)) {
    aplot <- aplot + geom_histogram(
      position = position,
      aes(color = !!sym(aes_color)),
      fill = bar_fill,
      size = bar_size,
      bins = bins,
      binwidth = binwidth,
      breaks = bin_breaks,
      alpha = bar_alpha,
      na.rm = silent_NA_warning)
  }else if(!is.null( aes_fill)){
    aplot <- aplot + geom_histogram(
      position = position,
      aes(fill = !!sym( aes_fill)),
      size = bar_size,
      color = bar_color,
      bins = bins,
      binwidth = binwidth,
      breaks = bin_breaks,
      alpha = bar_alpha,
      na.rm = silent_NA_warning)
  }else {
    aplot <- aplot + geom_histogram(
      position = position,
      fill = bar_fill,
      color = bar_color,
      size = bar_size,
      bins = bins,
      binwidth = binwidth,
      breaks =bin_breaks,
      alpha = bar_alpha,
      na.rm = silent_NA_warning)
  }

  if(plot_obs){
    if(plot_obs_jitter){
      aplot <- aplot +
        geom_rug(aes(y = 0), position = position_jitter(height = 0), color = plot_obs_color, length = grid::unit(plot_obs_len, "npc"))
    } else {
      aplot <- aplot +
        geom_rug(aes(y = 0), position = "identity", color = plot_obs_color, length = grid::unit(plot_obs_len, "npc"))
    }
  }

  if(bar_labels){
    if(do_coord_flip){
      aplot <- aplot + stat_bin(
        bins = bins,
        binwidth = binwidth,
        breaks = bin_breaks,
        geom = "text",
        color = bar_label_color,
        size = bar_label_size,
        aes(label = ..count..),
        hjust = -0.1,
        na.rm = silent_NA_warning)
    }else {
      aplot <- aplot + stat_bin(
        bins = bins,
        binwidth = binwidth,
        breaks = bin_breaks,
        geom = "text",
        color = bar_label_color,
        size = bar_label_size,
        aes(label = ..count..),
        vjust = -0.6,
        na.rm = silent_NA_warning)
    }
  }

  if(!is.null(palette_colors)){
    if(!is.null(aes_fill)) {
      aplot <- aplot + scale_fill_manual(values = palette_colors)
    }else if(!is.null(aes_color)) {
      aplot <- aplot + scale_color_manual(values = palette_colors)
    }
  }

  if(do_coord_flip){
    aplot <- aplot + coord_flip()
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

  aplot <- aplot + labs(title = title, subtitle = subtitle)

  aplot <- aplot + scale_x_continuous(
    limits = x_limits,
    breaks = x_major_breaks,
    minor_breaks = x_minor_breaks,
    labels = x_labels
  )

  aplot <- aplot + scale_y_continuous(
    limits = y_limits,
    breaks = y_major_breaks,
    minor_breaks = y_minor_breaks,
    labels = y_labels
  )

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
