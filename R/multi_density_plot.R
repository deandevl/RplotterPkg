#' Produces multiple ggplot2 based density plots.
#'
#' @description The function creates multiple density plots from one of two sources:
#'
#' 1. From a vector containing selected names of a data frame's variables.
#'
#' 2. A single variable's densities mapped across levels of a data frame's factor variable.
#'
#' The function returns a single multi-paneled plot object for display.
#'
#' @param df The target data frame for the density chart.
#' @param kernel A string that set the type of Kernel Density Estimation (KDE). Acceptable values are "gaussian",
#'  "rectangular", "triangular", "epanechnikov", "biweight", "cosine" or "optcosine".
#' @param bw  A string or numeric that sets the smoothing bandwidth to be used with the KDE function.
#' @param variables A vector that names the variables from \code{df} for plotting densities.
#' @param factor_var A string that sets the factor variable from \code{df} with multiple levels.
#' @param factor_x A string that sets a variable name from \code{df}.  If \code{factor_var} is not NULL
#'  then a density distribution of \code{factor_x} will be plotted for each of \code{factor_var}'s levels.
#' @param columns An integer the sets the number of columns for the multi-panel display.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill A string that sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param position A string that sets the position.  Acceptable values are \code{identity} which overlays or
#'  \code{stack} which stacks.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title for all variables in \code{variables}.
#' @param y_titles A string vector that sets the y axis title corresponding to each variable in \code{variables} or
#'  each factor level in \code{factor_var}. If \code{y_titles} is \code{NULL} then the y axis title is \dQuote{Density}.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param density_size A numeric that sets the density line width.
#' @param density_fill A string that sets the fill color for the density curve.
#' @param density_color A string that sets the color for the density line.
#' @param density_alpha A numeric that set the alpha component to \code{density_color}.
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
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param add_ons Is a vector of ggplot objects that will be appended to separate scatter plots. Each
#'  element in this vector addresses a separate plot in the order of presentation.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param plot_obs A logical which if \code{TRUE} plots a line for each observation along the axis margin.
#' @param plot_obs_len A numeric that sets the length of the \code{plot_obs} lines.
#' @param plot_obs_color A string that sets the color of the \code{plot_obs} lines.
#' @param plot_obs_jitter A logical which if \code{TRUE} will add a slight horizontal adjustment to overlapping observations.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param na.rm A logical which if \code{TRUE}, missing values are removed from \code{df}. If \code{FALSE} any missing
#'  values cause an error.
#' @param display_plot A logical that if TRUE will display the plot.
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
multi_density_plot <- function(
  df,
  kernel = "gaussian",
  bw = "nrd0",
  variables = NULL,
  factor_var = NULL,
  factor_x = NULL,
  columns = 2,
  aes_color = NULL,
  aes_fill = NULL,
  position = "stack",
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_title = NULL,
  y_titles = NULL,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  density_size = 1.0,
  density_fill = NA,
  density_color = "black",
  density_alpha = 1.0,
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
  col_width = 4,
  row_height = 4,
  add_ons = NULL,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  plot_obs = FALSE,
  plot_obs_len = 0.02,
  plot_obs_color = "black",
  plot_obs_jitter = FALSE,
  show_legend = FALSE,
  legend_pos = "top",
  na.rm = TRUE,
  display_plot = TRUE){

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

    if(!is.null(y_titles)){
      y_title <- y_titles[variable_id]
    }

    p1 <- create_density_plot(
      df = plot_df,
      kernel = kernel,
      bw = bw,
      aes_x = plot_x,
      aes_color = aes_color,
      aes_fill = aes_fill,
      position = position,
      title = plot_title,
      subtitle = NULL,
      x_title = x_title,
      y_title = y_title,
      rot_x_tic_angle = rot_x_tic_angle,
      rot_y_tic_label = rot_y_tic_label,
      density_size = density_size,
      density_fill = density_fill,
      density_color = density_color,
      density_alpha = density_alpha,
      palette_colors = palette_colors,
      x_limits = x_limits,
      x_major_breaks = x_major_breaks,
      x_minor_breaks = x_minor_breaks,
      x_labels = x_labels,
      y_limits = y_limits ,
      y_major_breaks = y_major_breaks,
      y_minor_breaks = y_minor_breaks,
      y_labels = y_labels,
      axis_text_size = axis_text_size,
      do_x_title = do_x_title,
      do_y_title = do_y_title,
      show_major_grids = show_major_grids,
      show_minor_grids = show_minor_grids,
      plot_obs = plot_obs,
      plot_obs_len = plot_obs_len,
      plot_obs_color = plot_obs_color,
      plot_obs_jitter = plot_obs_jitter,
      show_legend = show_legend,
      legend_pos = legend_pos,
      na.rm = na.rm
    )

    if(!is.null(add_on)) {
      p1 <- p1 + add_on
    }
    return(p1)
  }

  df_copy <- data.table::as.data.table(df)

  if(!is.null(factor_var)){
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
        plot_df = df,
        plot_x = variables[[i]],
        plot_title = variables[[i]],
        add_on = plot_add_ons[[i]])
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
