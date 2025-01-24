#' @title create_histogram_plot
#'
#' @description Function plots a ggplot2 based histogram with options for scaling and viewing observation locations. The function
#'   offers one of four ways of setting the number of bins.
#'
#' @param df The target data frame for the bar chart.
#' @param aes_x A required string that sets the x axis continuous variable name from 'df'.
#' @param aes_color Sets the variable name from 'df' for the aesthetic mapping for color.
#' @param aes_fill Sets the variable name from 'df' for the aesthetic mapping for fill.
#' @param position A string that sets the bar positions.  Acceptable values are "dodge", "dodge2"(side by side),
#'   "identity"(overlap) or "stack".
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the 'title' and 'subtitle'.
#' @param x_title A string that sets the x axis title. If \code{NULL} (the default) then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If \code{NULL} (the default) then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param bins An integer that sets the number of bins of the histogram. Is overridden by a non-null value for either 'binwidth',
#'  'bin_breaks' or 'bins_class'. Default is 20.
#' @param binwidth A numeric that sets the number of bins based on this value.  If 'aes_x' is a
#'  date variable then 'binwidth' is the number of days and if a time variable then 'binwidth' is
#'  the number of seconds.
#' @param bin_breaks A numeric vector that sets the number of bins by giving the bin boundaries explicitly.
#' @param bin_class A character string that sets the number of bins by selecting one of three types of formulas. Acceptable values are
#'  "Sturges", "Scott", or "FD".
#' @param bar_fill A string that sets the fill color attribute for the bars.
#' @param bar_color A string that sets the outline color attribute for the bars.
#' @param bar_alpha A numeric that set the alpha component attribute to 'bar_color'.
#' @param bar_lwd A numeric that sets the bar's outline line width attribute.
#' @param x_limits A numeric 2 element vector or function that sets the minimum and maximum for the x axis.  Use NA to
#'  refer to the existing minimum and maximum.
#' @param x_major_breaks A numeric vector or function that sets the major tic locations along the x axis.
#' @param x_minor_breaks A numeric vector or function that sets the minor tic locations along the x axis.
#' @param x_labels A character vector or function giving x axis tic labels.  Must be the same length as 'x_major_breaks'.
#' @param x_decimals A numeric that sets the number of decimal places for x-tic labels.
#' @param x_scientific A logical which if \code{TRUE} will put the x-tic labels in scientific notation.
#' @param x_log10 A logical which if \code{TRUE} will use a log10 scale for the x axis.
#' @param y_limits A numeric 2 element vector or function that sets the minimum and maximum for the y axis.  Use NA to
#'  refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that sets the major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that sets the minor tic locations along the y axis.
#' @param y_labels A character vector or function giving y axis tic labels.  Must be the same length as 'y_major_breaks'.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param do_coord_flip A logical which if TRUE will flip the x and y axis'.
#' @param bar_labels A logical which if \code{TRUE} will label each bar with its value.
#' @param bar_label_size A numeric that sets the size of the bar labels
#' @param bar_label_color A string that sets the color of the bar labels
#' @param plot_obs A logical which if \code{TRUE} plots a line for each observation along the axis margin.
#' @param plot_obs_len A numeric that sets the length of the 'plot_obs' lines.
#' @param plot_obs_color A string that sets the color of the 'plot_obs' lines.
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
#' library(rlang)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_histogram_plot(
#'   df = RplotterPkg::midwest,
#'   aes_x = "Area",
#'   binwidth = 0.01,
#'   x_limits = c(0.0, 0.11),
#'   x_major_breaks = seq(0.0, 0.11, 0.01),
#'   title = "Distribution of area",
#'   subtitle = "437 counties from midwest dataset",
#'   x_title = "Area",
#'   y_title = "Count",
#'   bar_color = "white",
#'   bar_lwd = 2.0,
#'   bar_fill = "brown",
#'   do_coord_flip = TRUE,
#'   bar_labels = TRUE,
#'   bar_label_size = 4,
#'   bar_label_color = "blue",
#'   rot_y_tic_label = TRUE,
#'   silent_NA_warning = TRUE,
#'   plot_obs = TRUE,
#'   plot_obs_color = "darkorange"
#' )
#'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @export
create_histogram_plot <- function(
    df,
    aes_x = NULL,
    aes_color = NULL,
    aes_fill = NULL,
    position = "stack",
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
    bins = 20,
    binwidth = NULL,
    bin_breaks = NULL,
    bin_class = NULL,
    bar_fill = NA,
    bar_color = "black",
    bar_alpha = 1.0,
    bar_lwd = 1.0,
    x_limits = NULL,
    x_major_breaks = waiver(),
    x_minor_breaks = waiver(),
    x_labels = waiver(),
    x_decimals = NULL,
    x_scientific = NULL,
    x_log10 = FALSE,
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
    show_major_grids = TRUE,
    show_minor_grids = TRUE,
    show_legend = TRUE,
    panel_color = "white",
    panel_border_color = "black",
    legend_pos = "top",
    legend_key_width = 0.7,
    legend_key_height = 0.7,
    legend_key_backgrd = "white",
    silent_NA_warning = FALSE,
    png_file_path = NULL,
    png_width_height = c(480,480)) {

  count <- NULL

  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
  }
  if(is.null(aes_x)){
    stop("aes_x is a required argument.")
  }

  if(!is.null(aes_fill)){
    aes_fill <- rlang::sym(aes_fill)
  }
  if(!is.null(aes_color)){
    aes_color <- rlang::sym(aes_color)
  }

  # -----------------Set geom attribute default values---------------
  ggplot2::update_geom_defaults(
    "bar",
    list(
      fill = bar_fill,
      color = bar_color,
      lwd = bar_lwd,
      alpha = bar_alpha
    )
  )

  # ---Has the user selected a bin class to redefine binwidth as a function------------------------------
  if(!is.null(bin_class)){
    fun_class <- switch(bin_class,
                        Sturges = grDevices::nclass.Sturges,
                        Scott = grDevices::nclass.scott,
                        FD = grDevices::nclass.FD,
                        stop("Unknown bin class")
    )
    binwidth <- function(x) {
      (max(x) - min(x)) / fun_class(x)
    }
  }

  # Convert df to a data.table
  dt <- data.table::as.data.table(df)

  # ------------------Define the main ggplot2 plot object/geoms------------
  aplot <- ggplot(
    data = dt,
    aes(
      x = !!sym(aes_x)
    )
  ) +
  geom_histogram(
    aes(
      color = !!aes_color,
      fill = !!aes_fill
    ),
    position = position,
    bins = bins,
    binwidth = binwidth,
    breaks = bin_breaks,
    alpha = bar_alpha,
    na.rm = silent_NA_warning
  )

  # ---Are we labeling the bars?----------------
  if(bar_labels){
    hjust <- 0.0
    vjust <- -0.6
    if(do_coord_flip){
      hjust <- -0.1
      vjust <- 0.0
    }
    aplot <- aplot + stat_bin(
      bins = bins,
      binwidth = binwidth,
      breaks = bin_breaks,
      geom = "text",
      color = bar_label_color,
      size = bar_label_size,
      aes(label = after_stat(count)),
      hjust = hjust,
      vjust = vjust,
      na.rm = silent_NA_warning
    )
  }

  # ---Are we plotting observations?---------------------------------------
  if(plot_obs){
    aplot <- aplot +
      geom_rug(aes(y = 0), position = "identity", color = plot_obs_color, length = grid::unit(plot_obs_len, "npc"))
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

  # ------------------Are we flipping the bars?---------------------
  if(do_coord_flip){
    aplot <- aplot + coord_flip()
  }

  # ---------------------------x/y tic scaling------------
  # ---x axis-----------------------------
  fmt_dcimals <- function(decimals=0, sci=FALSE){
    function(x) {format(x,nsmall = decimals,scientific=sci)}
  }

  if(!is.null(x_decimals) | !is.null(x_scientific)){
    if(is.null(x_decimals)){
      x_decimals <- 0
    }
    if(is.null(x_scientific)){
      x_scientific <- FALSE
    }
    x_labels <- fmt_dcimals(x_decimals, x_scientific)
  }

  if(x_log10){
    aplot <- aplot + scale_x_log10(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
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
  # ---y axis-------------------------------------
  aplot <- aplot + scale_y_continuous(
    limits = y_limits,
    breaks = y_major_breaks,
    minor_breaks = y_minor_breaks,
    labels = y_labels
  )

  # x/y tic label size, color, rotation
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

  # --------------------x/y axis titles----------------------------
  aplot <- aplot + ylab(y_title) + xlab(x_title)

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
