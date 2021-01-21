#' Produces multiple bar plots.
#'
#' @description The function creates multiple bar plots from one of two sources:
#'
#' 1. From a vector containing selected names of a data frame's variables.
#'
#' 2. A single variable's bar plot mapped across another variable's factor levels.
#'
#' The function returns a single multi-paneled plot object for display.
#'
#' @param df The source data frame for the bar chart.
#' @param variables A vector that names the variables from \code{df} for plotting bar charts.
#' @param factor_var A string that sets the factor variable from \code{df} with multiple levels.
#' @param factor_x A string that sets a variable name from \code{df}.  If \code{factor_var} is not NULL
#'  then a bar chart of \code{factor_x} will be plotted for each of \code{factor_var}'s levels.
#' @param columns An integer the sets the number of columns for the multi-panel display.
#' @param aes_y A string that sets the y axis variable name from \code{df}.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill A string that sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param position A string that sets the bar positions. Acceptable values are \code{dodge}(side by side),
#'  \code{identity}(overlap) or \code{stack}.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param x_title A string that sets the x axis title.
#' @param y_titles A string vector that sets the y axis title corresponding to each variable in \code{variables} or
#'  each factor level in \code{factor_var}. If \code{y_titles} is \code{NULL} then the y axis title is \dQuote{Count}.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param bar_fill A string that sets the fill color for the bars.
#' @param bar_color A string that sets the outline color for the bars.
#' @param bar_alpha A numeric that sets the alpha component to \code{bar_color}.
#' @param bar_size A numeric that sets the outline thickness of the bars.
#' @param bar_width A numeric that sets the width of the bars.
#' @param palette_colors A character vector to set the palette colors.
#' @param x_limits A character vector that defines possible values of the scale and their order..
#' @param x_breaks A character vector of x axis breaks or a function that takes the limits as input and returns breaks
#'  as output.
#' @param x_labels A character vector that defines the x axis tic labels. Vector must be the same length as
#'  \code{x_breaks}.
#' @param x_date_breaks If the class of \code{variables} or \code{factor_x} is Date/POSIXct, a string containing the number and date unit for
#'  major breaks. \code{"1 years"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels If the class of \code{variables} or \code{factor_x} is Date/POSIXct, a string containing the format codes, the
#'  strftime format, for the date. Examples: \code{\%Y-\%m}, \code{\%Y/\%b/\%d}, \code{\%H-\%M-\%S}
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
#' @param order_bars A string which will order the bars in a specific direction. Acceptable values are \dQuote{asc} or \dQuote{desc}
#' @param bar_labels A logical which if \code{TRUE} will label each bar with its value.
#' @param bar_label_size A numeric that sets the size of the bar labels
#' @param bar_label_color A string that sets the color of the bar labels
#' @param add_ons Is a vector of ggplot objects that will be appended to each bar plot object.  This can be used
#'  to override or add to all the multiple bar plot objects.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#'  are removed.
#' @param display_plot A logical that if TRUE will display the plot
#'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @import ggplot2
#'
#' @return A TableGrob object.
#'
#' @author Rick Dean
#'
#' @export
multi_bar_plot <- function(
  df,
  variables = NULL,
  factor_var = NULL,
  factor_x = NULL,
  columns = 2,
  aes_y = NULL,
  aes_color = NULL,
  aes_fill = NULL,
  position = "stack",
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_titles = NULL,
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
  col_width = 4,
  row_height = 4,
  do_coord_flip = FALSE,
  order_bars = NULL,
  bar_labels = FALSE,
  bar_label_size = 6,
  bar_label_color = "black",
  add_ons = NULL,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_legend = FALSE,
  legend_pos = "top",
  silent_NA_warning = FALSE,
  display_plot = TRUE) {

  plot_fun <- function(variable_id, n_variables, plot_df, plot_x, plot_title){
    if(columns == 1){
      do_y_title <-  TRUE
      if(variable_id == n_variables){
        do_x_title <- TRUE
      }else{
        do_x_title <- FALSE
      }
    }else{
      if(variable_id %% columns == 1){
        do_x_title <- TRUE
        do_y_title <- TRUE
      }else if(do_coord_flip){
        if(variable_id %% columns != 1){
          do_x_title <- TRUE
          do_y_title <- FALSE
        }
      }else {
        do_x_title <- TRUE
        do_y_title <- FALSE
      }
    }
    y_title <- "Count"
    if(!is.null(aes_y)) {
      y_title <- aes_y
    }else if(!is.null(y_titles)){
      y_title <- y_titles[variable_id]
    }
    p1 <- RplotterPkg::create_bar_plot(
      df = plot_df,
      aes_x = plot_x,
      aes_y = aes_y,
      aes_color = aes_color,
      aes_fill = aes_fill,
      position = position,
      title = plot_title,
      subtitle = NULL,
      x_title = x_title,
      y_title = y_title,
      rot_x_tic_angle = rot_x_tic_angle,
      rot_y_tic_label = rot_y_tic_label,
      bar_fill = bar_fill,
      bar_color = bar_color,
      bar_alpha = bar_alpha,
      bar_size = bar_size,
      bar_width = bar_width,
      palette_colors = palette_colors,
      x_limits = NULL,
      x_breaks = x_breaks,
      x_labels = x_labels,
      x_date_breaks = x_date_breaks,
      x_date_labels = x_date_labels,
      y_limits = y_limits,
      y_major_breaks = y_major_breaks,
      y_minor_breaks = y_minor_breaks,
      y_labels = y_labels,
      axis_text_size = axis_text_size,
      do_coord_flip = do_coord_flip,
      order_bars = order_bars,
      bar_labels = bar_labels,
      bar_label_size = bar_label_size,
      bar_label_color = bar_label_color,
      do_x_title = do_x_title,
      do_y_title = do_y_title,
      show_major_grids = show_major_grids,
      show_minor_grids = show_minor_grids,
      show_legend = show_legend,
      legend_pos = legend_pos,
      silent_NA_warning = silent_NA_warning
    )
    if(!is.null(add_ons)) {
      for(i in seq(from = 1, to = length(add_ons), by = 1)){
        p1 <- p1 + add_ons[i]
      }
    }
    return(p1)
  }

  df_copy <- data.table::as.data.table(df)

  if(!is.null(factor_var)){
    factor_levels <- levels(factor(df_copy[[factor_var]]))
    factor_n <- length(factor_levels)
    plots <- vector(mode = "list", length = factor_n)
    for(i in seq(1, factor_n, 1)){
      factor_level <- factor_levels[[i]]
      level_df <- df_copy[base::get(factor_var) == factor_level]
      plots[[i]] <- plot_fun(
        variable_id = i,
        n_variables = factor_n,
        plot_df = level_df,
        plot_x = factor_x,
        plot_title = factor_level)
    }
  }else{
    plots <- vector(mode = "list", length = length(variables))
    for(i in seq(1, length(variables), 1)){
      plots[[i]] <- plot_fun(
        variable_id = i,
        n_variables = length(variables),
        plot_df = df_copy,
        plot_x = variables[[i]],
        plot_title = variables[[i]])
    }
  }

  do_legend <- FALSE
  if(!is.null(aes_color) | !is.null(aes_fill)){
    do_legend <- TRUE
  }

  cols <- c()
  for(i in seq(1, length(plots), by = 1)){
    val <- i %% columns
    if(val == 0){
      cols <- c(cols, columns)
    }else {
      cols <- c(cols,val)
    }
  }
  n_rows <- ceiling(length(plots)/columns)
  rows <- c()
  for(i in seq(1, n_rows, by = 1)){
    for(ii in seq(1, columns, by = 1)){
      rows <- c(rows, i)
    }
  }
  layout <- list(
    plots = plots,
    rows = rows,
    cols = cols
  )
  multi_plot <- RplotterPkg::multi_panel_grid(
    layout = layout,
    col_widths = rep(col_width, columns),
    row_heights = rep(row_height, n_rows),
    title = title,
    subtitle = subtitle,
    do_legend = show_legend,
    display_plot = display_plot
  )
 return(multi_plot)
}
