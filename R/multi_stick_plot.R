#' Produces multiple stick plots.
#'
#' @description The function creates multiple histogram plots from one of two sources:
#'
#' 1. From a vector containing selected names of a data frame's variables.
#'
#' 2. A single variable's histograms mapped across levels of a data frame's factor variable.
#'
#' The function returns a single multi-paneled plot object for display.
#'
#' @param df The source data frame from which the sticks are plotted.
#' @param variables A vector that names the variables from \code{df} for the x axis.
#' @param factor_var A string that sets the factor variable from \code{df} with multiple levels.
#' @param factor_x A string that sets a variable name from \code{df}.  If \code{factor_var} is not NULL
#'  then a stick plot of \code{factor_x} will be plotted for each of \code{factor_var}'s levels.
#' @param columns An integer the sets the number of columns for the multi-panel display.
#' @param base_val A numeric that sets the base value from which the \dQuote{stick} originates.
#'  The default value is 0.
#' @param aes_y A string that sets the y axis variable name from \code{df} and controls the height of
#'  individual \dQuote{sticks}.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the x axis title.
#' @param y_titles A string vector that sets the y axis title corresponding to each factor level. If \code{y_titles}
#'  is \code{NULL} then the y axis title is the same as \code{aes_y}.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Depending on the class of \code{variables}, a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of \code{variables}, a numeric/Date/POSIXct vector or function that
#'  defines the exact major tic locations along the x axis.
#' @param x_labels A character vector with the same length as \code{x_major_breaks}, that labels the major tics.
#' @param x_major_date_breaks If the class of \code{variables} is Date/POSIXct, a string containing the number and date
#'  unit for major breaks. \code{"1 year"}, \code{"4 sec"}, \code{"3 month"}, \code{"2 week"}.
#' @param x_date_labels If the class of \code{variables} is Date/POSIXct, a string containing the format codes, the
#'  strftime format, for the date. Examples: \code{\%Y-\%m}, \code{\%Y/\%b/\%d}, \code{\%H-\%M-\%S}
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param line_color A string that sets the color of the lines.
#' @param line_size A numeric value that sets the thickness of lines.
#' @param palette_colors A character vector to set the palette colors.
#' @param add_ons Is a vector of ggplot objects that will be appended to separate scatter plots. Each
#'  element in this vector addresses a separate plot in the order of presentation.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param display_plot A logical that if TRUE will display the plot
#' @param bold_y A numeric that plots a bold horizontal line at this y value.
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
multi_stick_plot <- function(
  df,
  variables = NULL,
  factor_var = NULL,
  factor_x = NULL,
  columns = 2,
  base_val = 0,
  aes_y = NULL,
  aes_color = NULL,
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_titles = NULL,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_labels = waiver(),
  x_major_date_breaks = waiver(),
  x_date_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  col_width = 4,
  row_height = 4,
  line_color = "black",
  line_size = 0.8,
  palette_colors = NULL,
  add_ons = NULL,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_legend = FALSE,
  legend_pos = "top",
  display_plot = TRUE,
  bold_y = NULL,
  silent_NA_warning = FALSE) {

  plot_fun <- function(columns, variable_id, n_variables, plot_df, plot_x, plot_title, add_on){
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

    p1 <- create_stick_plot(
      df = plot_df,
      base_val = base_val,
      aes_x = plot_x,
      aes_y = aes_y,
      aes_color = aes_color,
      title = plot_title,
      subtitle = NULL,
      x_title = x_title,
      y_title = y_title,
      rot_x_tic_angle = rot_x_tic_angle,
      rot_y_tic_label = rot_y_tic_label,
      x_limits = x_limits,
      x_major_breaks = x_major_breaks,
      x_labels = x_labels,
      x_major_date_breaks = x_major_date_breaks,
      x_date_labels = x_date_labels,
      y_limits = y_limits,
      y_major_breaks = y_major_breaks,
      y_labels = y_labels,
      axis_text_size = axis_text_size,
      line_color = line_color,
      line_size = line_size,
      palette_colors = palette_colors,
      do_x_title = do_x_title,
      do_y_title = do_y_title,
      show_major_grids = show_major_grids,
      show_minor_grids = show_minor_grids,
      show_legend = show_legend,
      legend_pos = legend_pos,
      bold_y = bold_y,
      silent_NA_warning = silent_NA_warning
    )
    if(!is.null(add_on)) {
      p1 <- p1 + add_on
    }
    return(p1)
  }

  df_copy <- data.table::as.data.table(df)

  if(!is.null(factor_var)){
    factor_levels <- levels(factor(df[[factor_var]]))
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
  }else{
    plots <- vector(mode = "list", length = length(variables))
    plot_add_ons <- vector(mode = "list", length = length(variables))
    if(!is.null(add_ons)){
      for(i in 1:length(add_ons)){
        plot_add_ons[[i]] <- add_ons[[i]]
      }
    }
    for(i in seq(1, length(variables), 1)){
      plots[[i]] <- plot_fun(
        columns = columns,
        variable_id = i,
        n_variables = length(variables),
        plot_df = df_copy,
        plot_x = variables[[i]],
        plot_title = variables[[i]],
        add_on = plot_add_ons[[i]])
    }
  }

  do_legend <- FALSE
  if(!is.null(aes_color)){
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
