#' Function creates a bar plot.
#'
#' @description  Function creates a ggplot2 based bar plot with optional scaling and ordering.
#'
#' @param df The target data frame for the bar chart.
#' @param aes_x A string that sets the x axis discrete variable name from \code{df}. This variable must be a factor.
#' @param aes_y A string that sets the y axis numeric variable name from \code{df}.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill A string that sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param position A string that sets the bar positions.  Acceptable values are \code{dodge}(side by side),
#'  \code{identity}(overlap) or \code{stack}.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic label. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param bar_fill A string that sets the fill color for the bars.
#' @param bar_color A string that sets the outline color for the bars.
#' @param bar_alpha A numeric that sets the alpha component to \code{bar_color}.
#' @param bar_size A numeric that sets the outline thickness of the bars.
#' @param bar_width A numeric that sets the width of the bars.
#' @param palette_colors A character vector to set the palette colors.
#' @param x_limits Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use NA to refer to the existing minimum and maximum..
#' @param x_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines the
#'  exact major tic locations along the x axis.
#' @param x_labels A character vector that defines the x axis tic labels. Vector must be the same length as
#'  \code{x_breaks}.
#' @param x_date_breaks If the class of \code{aes_x} is Date/POSIXct, a string containing the number and date unit for
#'  major breaks. \code{"1 years"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels If the class of \code{aes_x} is Date/POSIXct, a string containing the format codes, the
#'  strftime format, for the date. Examples: \code{\%Y-\%m}, \code{\%Y/\%b/\%d}, \code{\%H-\%M-\%S}
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
#' @param order_bars A string which will order the bars in a specific direction. Acceptable values are \dQuote{asc} or \dQuote{desc}
#' @param bar_labels A logical which if \code{TRUE} will label each bar with its value.
#' @param bar_label_size A numeric that sets the size of the bar labels
#' @param bar_label_color A string that sets the color of the bar labels
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#'  are removed.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table setorderv
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @return A plot object.
#'
#' @author Rick Dean
#'
#' @export
create_bar_plot <- function(
  df,
  aes_x,
  aes_y = NULL,
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
  bar_fill = NA,
  bar_color = "black",
  bar_alpha = 1.0,
  bar_size = 1.0,
  bar_width = NULL,
  palette_colors = NULL,
  x_limits = NULL,
  x_breaks = waiver(),
  x_labels = waiver(),
  x_date_breaks = waiver(),
  x_date_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  do_coord_flip = FALSE,
  order_bars = NULL,
  bar_labels = FALSE,
  bar_label_size = 4,
  bar_label_color = "black",
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  do_x_title = TRUE,
  do_y_title = TRUE,
  show_legend = TRUE,
  legend_pos = "top",
  silent_NA_warning = FALSE) {

  df_copy <- data.table::as.data.table(df)

  if(is.null(aes_y)) {
    stat <- "count"
    if(!is.null(order_bars)){
      if(order_bars == "desc"){
        x_ordered_df <- df_copy[, .(N = .N), by = aes_x][order(-N)]
      }else{
        x_ordered_df <- df_copy[, .(N = .N), by = aes_x][order(N)]
      }
      col_x <- factor(df_copy[[aes_x]], levels = x_ordered_df[[aes_x]])
      df_copy[, c(aes_x) := col_x]
    }
    aplot <- ggplot(data = df_copy, aes(x = !!rlang::sym(aes_x)))
    if(bar_labels){
      if(do_coord_flip){
        aplot <- aplot + geom_text(
          stat = "count",
          aes(label = stat(count), hjust = -0.1),
          color = bar_label_color,
          size = bar_label_size,
          na.rm = silent_NA_warning
        )
      }else {
        aplot <- aplot + geom_text(
          stat = "count",
          aes(label = stat(count), vjust = -0.3),
          color = bar_label_color,
          size = bar_label_size,
          na.rm = silent_NA_warning
        )
      }
    }
  }else {
    stat <- "identity"
    if(!is.null(order_bars)){
      if(order_bars == "desc"){
        data.table::setorderv(df_copy, cols = aes_y, order = -1L)
      }else {
        data.table::setorderv(df_copy, cols = aes_y, order = 1L)
      }
      col_x <- factor(df_copy[[aes_x]], levels = df_copy[[aes_x]])
      df_copy[, c(aes_x) := col_x]
    }
    aplot <- ggplot(data = df_copy, aes(x = !!rlang::sym(aes_x), y = !!rlang::sym(aes_y)))
    if(bar_labels){
      if(do_coord_flip){
        aplot <- aplot + geom_text(
          aes(label = !!rlang::sym(aes_y), hjust = -0.1),
          color = bar_label_color,
          size = bar_label_size,
          na.rm = silent_NA_warning
        )
      }else {
        aplot <- aplot + geom_text(
          aes(label = !!rlang::sym(aes_y), vjust = -0.3),
          color = bar_label_color,
          size = bar_label_size,
          na.rm = silent_NA_warning
        )
      }
    }
  }

  if(!is.null(aes_color)) {
    aplot <- aplot + geom_bar(stat = stat, position = position, aes(color = !!rlang::sym(aes_color)), fill = bar_fill,
                            size = bar_size, width = bar_width,  alpha = bar_alpha, show.legend = show_legend, na.rm = silent_NA_warning)
  }else if(!is.null(aes_fill)){
    aplot <- aplot + geom_bar(stat = stat, position = position, aes(fill = !!rlang::sym(aes_fill)), color = bar_color,
                            size = bar_size, width = bar_width, alpha = bar_alpha, show.legend = show_legend, na.rm = silent_NA_warning)
  }else{
    aplot <- aplot + geom_bar(stat = stat, position = position, fill = bar_fill, color = bar_color,
                            size = bar_size, width = bar_width, alpha = bar_alpha, na.rm = silent_NA_warning)
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

  if(is(df_copy[[aes_x]], "Date")) {
    aplot <- aplot + scale_x_date(
      limits = x_limits,
      date_breaks = x_date_breaks,
      date_labels = x_date_labels
    )
  }else if(is(df_copy[[aes_x]],"POSIXct") || is(df_copy[[aes_x]],"POSIXlt")) {
    aplot <- aplot + scale_x_datetime(
      limits = x_limits,
      date_breaks = x_date_breaks,
      date_labels = x_date_labels
    )
  }else if(is(df_copy[[aes_x]], "difftime") || is(df_copy[[aes_x]], "hms")){
    aplot <- aplot + scale_x_time(
      limits = x_limits
    )
  }else if(is(df_copy[[aes_x]], "numeric")){
    aplot <- aplot + scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks,
      labels = x_labels
    )
  }else {
    aplot <- aplot + scale_x_discrete(
      limits = x_limits,
      breaks = x_breaks,
      labels = x_labels
    )
  }

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
