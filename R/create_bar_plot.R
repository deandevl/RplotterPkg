#' @title create_bar_plot
#'
#' @description Function wraps ggplot2 geom_bar and geom_col to creates a bar plot.
#'
#' @param df The target data frame for the bar chart.
#' @param aes_x Sets the x axis discrete variable name from \code{df}. This variable must be a factor
#'   and is required..
#' @param aes_y Sets the y axis numeric variable name from \code{df}. If this variable is NULL
#'   the bar heights will be proportional to the number of cases in the levels of \code{aes_x}.
#'   If \code{aes_y} is not NULL, the bar heights represent this parameter's values across the \code{aes_x} levels.
#' @param aes_color Sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill Sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param position A string that sets the bar positions.  Acceptable values are \code{dodge}(side by side),
#'  \code{identity}(overlap) or \code{stack}.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param caption A string that sets the caption.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title. If NULL (the default)  then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If NULL (the default)  then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic label. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param bar_fill A string that sets the fill color attribute for the bars.
#' @param bar_color A string that sets the outline color attribute for the bars.
#' @param bar_alpha A numeric that sets the alpha component attribute to \code{bar_color}.
#' @param bar_lwd A numeric that sets the bar's outline line width attribute.
#' @param bar_width A numeric that sets the width attribute of the bars.
#' @param x_major_breaks If \code{aes_x} is numeric then this parameter is a numeric vector that defines the major
#'  breaks/intervals for the x axis.  Interval labels are created and their respective counts are displayed.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
#' @param order_bars A string which will order the bars in a specific direction. Acceptable values are \code{asc} or \code{desc}
#' @param bar_labels A logical which if \code{TRUE} will label each bar with its value.
#' @param bar_label_sz A numeric that sets the size of the label.
#' @param bar_label_color A string that sets the label's color.
#' @param bar_label_fontface A string that sets the label's font face. Acceptable values are "plain", "bold",
#'  "italic", "bold.italic".
#' @param bar_label_alpha A numeric that sets the label's alpha value.
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
#' library(RColorBrewer)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_bar_plot(
#'   df = RplotterPkg::religion,
#'   aes_x = "happy",
#'   aes_fill = "religion",
#'   position = "dodge",
#'   rot_y_tic_label = TRUE,
#'   bar_color = "black",
#'   bar_lwd = 2,
#'   bar_width = 0.8,
#'   x_title = "Happiness",
#'   y_title = "Count",
#'   axis_text_size = 16
#' ) +
#' ggplot2::scale_fill_discrete(
#'   type = RColorBrewer::brewer.pal(n = 9, name = "Set1")
#' )
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table setorderv
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @export
create_bar_plot <- function(
    df = NULL,
    aes_x = NULL,
    aes_y = NULL,
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
    bar_fill = NA,
    bar_color = "black",
    bar_alpha = 1.0,
    bar_lwd = 1.0,
    bar_width = NULL,
    x_major_breaks = NULL,
    y_limits = NULL,
    y_major_breaks = waiver(),
    y_minor_breaks = waiver(),
    y_labels = waiver(),
    axis_text_size = 11,
    do_coord_flip = FALSE,
    order_bars = NULL,
    bar_labels = FALSE,
    bar_label_sz = 4,
    bar_label_color = "black",
    bar_label_fontface = "plain",
    bar_label_alpha = 1.0,
    show_major_grids = TRUE,
    show_minor_grids = TRUE,
    panel_color = "white",
    panel_border_color = "black",
    show_legend = TRUE,
    legend_pos = "right",
    legend_key_width = 0.7,
    legend_key_height = 0.7,
    legend_key_backgrd = "white",
    silent_NA_warning = FALSE,
    png_file_path = NULL,
    png_width_height = c(480,480)) {

  N <- count <- . <- ..aes_x <- NULL

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
  # -------------Set geom attribute default values------------------
  ggplot2::update_geom_defaults(
    "bar",
    list(
      fill = bar_fill,
      color = bar_color,
      lwd = bar_lwd,
      alpha = bar_alpha
    )
  )

  ggplot2::update_geom_defaults(
    "col",
    list(
      fill = bar_fill,
      color = bar_color,
      lwd = bar_lwd,
      alpha = bar_alpha
    )
  )

  ggplot2::update_geom_defaults(
    "text",
    list(
      size = bar_label_sz,
      color = bar_label_color,
      fontface = bar_label_fontface,
      alpha = bar_label_alpha
    )
  )

  # Convert df to a data.table
  dt <- data.table::as.data.table(df)

  # ------------Reorder the data.frame aes_x factor levels?--------------
  if(!is.null(order_bars) & is.null(aes_y) & is.null(x_major_breaks)){
    # We are doing bars that reflect counts
    # 1. Group by aes_x to find the counts for each subgroups N
    # 2. Order by the N counts
    # 3. Sets the levels of aes_x by this ordering
    aes_count_ordered_dt <- NULL
    if(order_bars == "desc"){
      aes_count_ordered_dt <- dt[, .(N = .N), by = aes_x][order(-N)]
    }else{
      aes_count_ordered_dt <- dt[, .(N = .N), by = aes_x][order(N)]
    }

    dt[[aes_x]] <- factor(dt[[aes_x]], levels = aes_count_ordered_dt[[aes_x]])
  }else if(!is.null(order_bars) & is.null(x_major_breaks)){
    # We are doing bars that reflect values
    # 1. Set the order of dt by aes_y
    # 2. Set the levels of aes_x by the re-ordered aes_x
    if(order_bars == "desc"){
      data.table::setorderv(dt, cols = aes_y, order = -1L)
    }else {
      data.table::setorderv(dt, cols = aes_y, order = 1L)
    }
    dt[[aes_x]] <- factor(dt[[aes_x]], levels = dt[[aes_x]])
  }

  # Is x_major_breaks defined?
  if(!is.null(x_major_breaks)){
    breaks_data_v <- dt[, ..aes_x][[1]]
    dt[, cut := cut(x = breaks_data_v, breaks = x_major_breaks, ordered_result = T)]
    aes_x <- "cut"
  }

  # ------------Define the main ggplot2 plot object, aesthetics, geoms------------
  aplot <- ggplot(
    data = dt
  )

  if(is.null(aes_y)){ # We are doing counts (i.e. geom_bar)
    aplot <- aplot +
      geom_bar(
        aes(
          !!sym(aes_x),
          fill = !!aes_fill,
          color = !!aes_color
        ),
        stat = "count",
        position = position,
        width = bar_width,
        na.rm = silent_NA_warning
      )
  }else { # We are doing values (i.e. geom_col)
    aplot <- aplot +
      geom_col(
        aes(
          !!sym(aes_x),
          !!sym(aes_y),
          fill =  !!aes_fill,
          color = !!aes_color
        ),
        position = position,
        width = bar_width,
        na.rm = silent_NA_warning
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

  # -------------------Are we labeling the bars--------------------
  if(bar_labels){
    hjust <- 0.0
    vjust <- -0.3
    if(do_coord_flip){
      hjust <- -0.1
      vjust <- 0.0
    }
    if(is.null(aes_y)){
      aplot <- aplot +
        ggplot2::geom_text(
          aes(
            !!sym(aes_x),
            label = after_stat(count),
            hjust = hjust,
            vjust = vjust
          ),
          stat = "count",
          na.rm = silent_NA_warning
        )
    }else {
      aplot <- aplot +
        ggplot2::geom_text(
          data = dt,
          stat = "identity",
          aes(
            !!sym(aes_x),
            !!sym(aes_y),
            label = !!sym(aes_y),
            hjust = hjust,
            vjust = vjust
          )
        )
    }
  }

  # ---------------y axis scaling-------------------
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

  # ------------------Are we flipping the bars?---------------------
  if(do_coord_flip){
    aplot <- aplot + coord_flip()
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
