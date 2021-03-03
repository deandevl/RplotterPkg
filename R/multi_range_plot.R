#' Produces multiple range plots.
#'
#' @description The function creates multiple range plots from x/y pairs across
#' the levels of a separate selected factor variable.
#'
#' @param df The source data frame from which the x/y pairs and factor variable originate.
#' @param factor_var A string that sets the factor variable from \code{df} whose levels will be plotted.
#' @param factor_x A string that sets a variable name from \code{df}. A range plot of
#'  \code{factor_x} versus \code{aes_y} will be plotted for each of \code{factor_var}'s levels.
#' @param columns An integer that sets the number of columns for the multi-panel display.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param aes_y A string that sets the y axis variable name from \code{df}.
#' @param aes_y_min A string that sets a y axis variable name from \code{df}.
#'  These are the required numerics that defines the minimum values for the range
#'  of \code{aes_y}.
#' @param aes_y_max A string that sets a y axis variable name from \code{df}.
#'  These are the required numerics that defines the maximum values for the range
#'  of \code{aes_y}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title}
#'  and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_titles A string vector that sets the y axis title corresponding to each factor level. If \code{y_titles}
#'  is \code{NULL} then the y axis title is the same as \code{aes_y}.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels.
#'  When x tic labels are long, a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees
#'  for enhanced readability.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param y_log10 A logical which if \code{TRUE} will use a log10 scale for the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param line_type A string that sets range line type \code{twodash, solid, longdash, dotted, dotdash,
#'  dashed, blank}.
#' @param line_pts_size A numeric value that sets the size of both lines(width) and points(diameter).
#' @param line_pts_color A string that sets the color of the range lines and outlines of the points.
#' @param line_pts_alpha A numeric value that sets the alpha level of points.
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
#' @param display_plot A logical that if TRUE will display the plot
#' @param add_ons Is a vector of ggplot objects that will be appended to separate scatter plots. Each
#'  element in this vector addresses a separate plot in the order of presentation.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#' are removed.
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
multi_range_plot <- function(
  df,
  factor_var = NULL,
  factor_x = NULL,
  columns = 2,
  col_width = 4,
  row_height = 4,
  aes_y = NULL,
  aes_y_min,
  aes_y_max,
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_title = NULL,
  y_titles = NULL,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  y_log10 = FALSE,
  axis_text_size = 11,
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  line_type = "solid",
  line_pts_size = 1,
  line_pts_color = "black",
  line_pts_alpha = 1.0,
  do_x_title = TRUE,
  do_y_title = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  display_plot = TRUE,
  do_coord_flip = FALSE,
  add_ons = NULL,
  silent_NA_warning = FALSE
){
  plot_fun <- function(columns, variable_id, n_variables, plot_df, plot_x, plot_title, add_on){

    y_title <-  aes_y
    if(!is.null(y_titles)){
      y_title <- y_titles[variable_id]
    }

    if(do_coord_flip){
      if(variable_id %% columns == 1){
        do_x_title <- FALSE
        do_y_title <- TRUE
      }else{
        do_x_title <- FALSE
        do_y_title <- FALSE
      }
    }else if(columns == 1){
      do_y_title <-  TRUE
      if(variable_id == n_variables){
        do_x_title <- TRUE
      }else{
        do_x_title <- FALSE
      }
    }else {
      if(variable_id %% columns == 1){
        do_x_title <- TRUE
        do_y_title <- TRUE
      }else {
        do_x_title <- TRUE
        do_y_title <- FALSE
      }
    }

    p1 <- create_range_plot(
      df = plot_df,
      aes_x = plot_x,
      aes_y = aes_y,
      aes_y_min = aes_y_min,
      aes_y_max = aes_y_max,
      title = plot_title,
      subtitle = NULL,
      x_title = x_title,
      y_title = y_title,
      rot_x_tic_angle = rot_x_tic_angle,
      rot_y_tic_label = rot_y_tic_label,
      y_limits = y_limits,
      y_major_breaks = y_major_breaks,
      y_minor_breaks = y_minor_breaks,
      axis_text_size = axis_text_size,
      y_log10 = y_log10,
      pts_fill = pts_fill,
      pts_shape = pts_shape,
      pts_stroke = pts_stroke,
      line_type = line_type,
      line_pts_size = line_pts_size,
      line_pts_color = line_pts_color,
      line_pts_alpha = line_pts_alpha,
      do_x_title = do_x_title,
      do_y_title = do_y_title,
      show_major_grids = show_major_grids,
      show_minor_grids = show_minor_grids,
      do_coord_flip = do_coord_flip,
      silent_NA_warning = silent_NA_warning
    )
    if(!is.null(add_on)) {
      p1 <- p1 + add_on
    }
    return(p1)
  }
  df_copy <- data.table::as.data.table(df)
  factor_levels <- levels(factor(df_copy[[factor_var]]))
  factor_n <- length(factor_levels)
  plots <- vector(mode = "list", length = factor_n)
  plot_add_ons <- vector(mode = "list", length = factor_n)
  if(!is.null(add_ons)){
    for(i in 1:length(add_ons)){
      plot_add_ons[[i]] <- add_ons[[i]]
    }
  }
  for(i in seq(1, factor_n, 1)){
    factor_level <- factor_levels[[i]]
    level_df <- df_copy[base::get(factor_var) == factor_level]
    plots[[i]] <- plot_fun(
      columns = columns,
      variable_id = i,
      n_variables = factor_n,
      plot_df = level_df,
      plot_x = factor_x,
      plot_title = factor_level,
      add_on = plot_add_ons[[i]])
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
    display_plot = display_plot
  )
  return(multi_plot)
}

