#' Produces multiple scatter plots.
#'
#' @description The function creates multiple scatter plots from one of two sources:
#'
#' 1. From a vector containing selected names of a data frame's variables.  These x axis variables
#'  are paired with a single y axis variable in multiple panels of x versus y scatter plots.
#'
#' 2. A single variable's scatter plot mapped across levels of a data frame's factor variable.
#'
#' The function returns a single multi-paneled plot object for display.
#'
#' @param df The source data frame from which the scatter points are plotted.
#' @param variables A vector that names the x axis variables from \code{df} for plotting these variables
#'  versus \code{aes_y} in multiple scatter plots.
#' @param factor_var A string that sets the factor variable from \code{df} whose levels will be plotted.
#' @param factor_x A string that sets a variable name from \code{df}.  If \code{factor_var} is not NULL
#'  then a scatter plot of \code{factor_x} versus \code{aes_y} will be plotted for each of \code{factor_var}'s levels.
#' @param columns An integer that sets the number of columns for the multi-panel display.
#' @param aes_y A string that sets the y axis variable name from \code{df}.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill A string that sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param aes_size A string that sets the variable name from \code{df} for the aesthetic mapping for size.
#' @param aes_linetype A string that sets the variable name from \code{df} for the aesthetic mapping for linetype.
#' @param aes_label A string that sets the variable name from \code{df} for the aesthetic mapping for labeling. If
#'  labelling points then the package \code{ggrepel} is required.
#' @param position A string or function that does a slight adjustment to overlapping points.  Typical values are
#'  \code{jitter} or \code{position_jitter(width = 0.1, height = 0.1)}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the x axis title.
#' @param y_titles A string vector that sets the y axis title corresponding to each factor level. If \code{y_titles}
#'  is \code{NULL} then the y axis title is the same as \code{aes_y}.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the
#'  minimum and maximum for the x axis.
#' @param x_major_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param x_minor_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines
#'  the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as \code{x_major_breaks}, that labels the major tics.
#' @param x_major_date_breaks If the class of \code{aes_x} is Date/POSIXct, a string containing the number and date unit
#'  for major breaks. Examples: \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_minor_date_breaks If the class of \code{aes_x} is Date/POSIXct, a string containing the number and date unit
#'  for minor breaks.
#' @param x_date_labels If the class of \code{aes_x} is Date/POSIXct, a string containing the format codes, the strftime
#'  format, for the date. Examples: \code{"\%Y-\%m"}, \code{"\%Y/\%b/\%d"}, \code{"\%H-\%M-\%S"}.
#' @param x_log10 A logical which if \code{TRUE} will use a log10 scale for the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param y_log10 A logical which if TRUE will use a log10 scale for the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param show_pts A logical which if FALSE will plot only the lines if \code{connect} is TRUE.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_size A numeric value that sets the size of the points.
#' @param pts_line_alpha A numeric value that sets the alpha level of \code{pts_line_color}.
#' @param palette_colors A string vector to set the aes mapping of colors which changes their appearance in the
#'  order given.
#' @param palette_linetypes A string vector to set the aes mapping of linetypes which changes their appearance in
#'  the order given.
#' @param connect A logical which if \code{TRUE} then points will be connected with a line.
#' @param line_size A numeric value that sets the thickness of lines if \code{connect} is TRUE.
#' @param line_color A string that sets the color of the lines if \code{connect} is TRUE.
#' @param connect_linetype A string that sets line type \code{twodash, solid, longdash, dotted, dotdash,
#'  dashed, blank} if \code{connect} is TRUE.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param display_plot A logical that if TRUE will display the plot
#' @param bold_y A numeric that plots a bold horizontal line at this y value intercept.
#' @param add_ons Is a vector of ggplot objects that will be appended to each scatter plot object.  This can be used
#'  to override or add to all the multiple scatter plot objects.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when NA's
#' are removed at the start/end of the line.
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
multi_scatter_plot <- function(
  df,
  variables = NULL,
  factor_var = NULL,
  factor_x = NULL,
  columns = 2,
  aes_y = NULL,
  aes_color = NULL,
  aes_fill = NULL,
  aes_size = NULL,
  aes_linetype = NULL,
  aes_label = NULL,
  position = ggplot2::position_jitter(width = 0.0, height = 0.0),
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_titles = NULL,
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
  axis_text_size = 11,
  show_pts = TRUE,
  y_log10 = FALSE,
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  pts_color = "black",
  pts_line_alpha = 1.0,
  pts_size = 1,
  palette_colors = NULL,
  palette_linetypes = NULL,
  connect = FALSE,
  line_size = 0.6,
  line_color = "black",
  connect_linetype = "solid",
  col_width = 4,
  row_height = 4,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_legend = FALSE,
  legend_pos = "top",
  display_plot = TRUE,
  bold_y = NULL,
  add_ons = NULL,
  silent_NA_warning = FALSE){

  plot_fun <- function(variable_id, n_variables, plot_df, plot_x, plot_title){
    if(columns == 1){
      do_y_title <-  TRUE
      if(variable_id == n_variables){
        do_x_title <- TRUE
      }else{
        do_x_title <- FALSE
      }
    }else{
      do_x_title <- TRUE
      if(variable_id %% columns == 1){
        do_y_title <- TRUE
      }else{
        do_y_title <- FALSE
      }
    }

    y_title <-  aes_y
    if(!is.null(y_titles)){
      y_title <- y_titles[variable_id]
    }

    p1 <- create_scatter_plot(
      df = plot_df,
      aes_x = plot_x,
      aes_y = aes_y,
      aes_color = aes_color,
      aes_fill = aes_fill,
      aes_size = aes_size,
      aes_linetype = aes_linetype,
      aes_label = aes_label,
      position = position,
      title = plot_title,
      subtitle = NULL,
      x_title = x_title,
      y_title = y_title,
      rot_x_tic_angle = rot_x_tic_angle,
      rot_y_tic_label = rot_y_tic_label,
      x_limits = x_limits,
      x_major_breaks = x_major_breaks,
      x_minor_breaks = x_minor_breaks,
      x_labels = x_labels,
      x_major_date_breaks = x_major_date_breaks,
      x_minor_date_breaks = x_minor_date_breaks,
      x_date_labels = x_date_labels,
      x_log10 = x_log10,
      y_limits = y_limits,
      y_major_breaks = y_major_breaks,
      y_minor_breaks = y_minor_breaks,
      y_labels = y_labels,
      axis_text_size = axis_text_size,
      show_pts = show_pts,
      y_log10 = y_log10,
      pts_fill = pts_fill,
      pts_shape = pts_shape,
      pts_stroke = pts_stroke,
      pts_color = pts_color,
      pts_line_alpha = pts_line_alpha,
      pts_size = pts_size,
      palette_colors = palette_colors,
      palette_linetypes = palette_linetypes,
      connect = connect,
      line_size = line_size,
      line_color = line_color,
      connect_linetype = connect_linetype,
      do_x_title = do_x_title,
      do_y_title = do_y_title,
      show_major_grids = show_major_grids,
      show_minor_grids = show_minor_grids,
      bold_y = bold_y,
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
  if(!is.null(aes_color) | !is.null(aes_fill) | !is.null(aes_size)){
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
