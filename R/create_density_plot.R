#' Function creates a density plot.
#'
#' @description Function creates a ggplot2 based density plot with options for scaling, shading probability areas,
#'  and plotting observation locations. The function's density arguments mirror most of the arguments available
#'  from \code{stats::density()} for the Kernel Density Estimation (KDE). See the \code{stats::density()}'s help page
#'  for more information.
#'
#' @param df A data frame that contains a numeric vector from which to estimate the KDE along with possible variables
#'  for setting \code{aes_color} and \code{aes_fill} (see below).
#' @param aes_x The variable from \code{df} from which to estimate the KDE.
#' @param bw  A string or numeric that sets the smoothing bandwidth to be used with the KDE function.
#' @param adjust A numeric that adjusts \code{bw} since the actual bandwidth is computed as \code{adjust*bw}.
#' @param kernel A string that set the type of Kernel Density Estimation (KDE). Acceptable values are "gaussian",
#'  "rectangular", "triangular", "epanechnikov", "biweight", "cosine" or "optcosine".
#' @param n The number of equally spaced points at which the density is to be estimated. This should be a power of two.
#' @param na.rm A logical which if \code{TRUE}, missing values are removed from \code{df}. If \code{FALSE} any missing
#'  values cause an error.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill A string that sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param position A string that sets the position.  Acceptable values are \code{identity} which overlays or
#'  \code{stack} which stacks.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param density_size A numeric that sets the density line width.
#' @param density_color A string that sets the color for the density line.
#' @param density_fill A string that sets the color for the density fill.
#' @param density_alpha A numeric that sets the alpha value for \code{density_fill}.
#' @param palette_colors A character vector to set the palette colors.
#' @param x_limits A numeric 2 element vector or function that sets the minimum and maximum for the x axis. Use NA to
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
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param plot_obs A logical which if \code{TRUE} plots a line for each observation along the axis margin.
#' @param plot_obs_len A numeric that sets the length of the \code{plot_obs} lines.
#' @param plot_obs_color A string that sets the color of the \code{plot_obs} lines.
#' @param plot_obs_jitter A logical which if \code{TRUE} will add a slight horizontal adjustment to overlapping observations.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param cum_prob A one or two element numeric vector for defining cumulative probabilities which divide the density plot area. The
#'  values are probabilities with values within [0.0, 1.0].  If for example with a cumulative probability \code{cum_prob} of .95, then the density is divided
#'  into two probability areas of 95% and 5% with the dividing quantile value labeled. Similarly if two cumulative probabilities are submitted,
#'  then three areas are defined.
#' @param area_colors A string vector that sets the color of each area defined by \code{cum_prob}.
#' @param area_quantile_line_color A string that sets the vertical line color at quantile locations that divide the areas defined
#'  by \code{cum_prob}.
#'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @return A ggplot2 plot object.
#'
#' @author Rick Dean
#'
#' @export
create_density_plot <- function(
  df,
  aes_x,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  n = 512,
  na.rm = TRUE,
  aes_color = NULL,
  aes_fill = NULL,
  position = "identity",
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_title = aes_x,
  y_title = "Density",
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  density_size = 1.0,
  density_color = "black",
  density_fill = "gray",
  density_alpha = 0.4,
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
  do_x_title = TRUE,
  do_y_title = TRUE,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  plot_obs = FALSE,
  plot_obs_len = 0.02,
  plot_obs_color = "black",
  plot_obs_jitter = FALSE,
  show_legend = TRUE,
  legend_pos = "top",
  cum_prob = NULL,
  area_colors = c("gray", "green", "gray"),
  area_quantile_line_color = "red"){

  df_copy <- data.table::as.data.table(df)

  if(!is.null(cum_prob)){
    a_density <- stats::density(
      x = df_copy[[aes_x]],
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      na.rm = na.rm
    )
    density_df <- data.table::data.table(
      x = a_density$x,
      y = a_density$y
    )

    out_list <- list()
    densities <- list()
    quantiles <- NULL
    areas <- c()
    if(length(cum_prob) == 1){
      quantiles <- stats::quantile(df_copy[[aes_x]], probs = cum_prob)
      subset_1 <- subset(density_df, x < quantiles[[1]])
      subset_2 <- subset(density_df, x >= quantiles[[1]])
      densities <- list(subset_1, subset_2)
      areas <- c(cum_prob, 1 - cum_prob)
    }else{
      quantiles <- stats::quantile(df_copy[[aes_x]], probs = cum_prob)
      subset_1 <- subset(density_df, x < quantiles[[1]])
      subset_2 <- subset(density_df, x >= quantiles[[1]] & x < quantiles[[2]])
      subset_3 <- subset(density_df, x >= quantiles[[2]])
      densities <- list(subset_1, subset_2, subset_3)
      areas <- round(c(cum_prob[[1]], cum_prob[[2]]-cum_prob[[1]], 1 - cum_prob[[2]]), digits = 3)
    }

    aplot <- ggplot()
    for(i in 1:length(densities)){
      densities_n <- nrow(densities[[i]])

      # plot area
      aplot <- aplot + geom_ribbon(
        data = densities[[i]],
        aes(x = x, ymin = 0, ymax = y),
        fill = area_colors[[i]],
        alpha = density_alpha,
        color = NA
      )

      if(i < length(densities)){
        # plot areas dividing line segment
        aplot <- aplot + geom_segment(
          data = densities[[i]],
          x = quantiles[[i]],
          y = 0,
          xend = quantiles[[i]],
          yend = densities[[i]]$y[[densities_n]],
          color = area_quantile_line_color,
          size = 1.4
        )
        # annotate quantile value above segment
        aplot <- aplot +
          annotate(
            geom = "text",
            label = round(quantiles[[i]],digits = 2),
            x = quantiles[[i]],
            y = densities[[i]]$y[[densities_n]],
            vjust = -0.2,
            fontface = "bold",
            size = 5
          )
      }

      # annotate area
      aplot <- aplot +
        annotate(
          geom = "text",
          label = paste0("area = ", areas[[i]]),
          color = "black",
          x = densities[[i]]$x[[0.5*densities_n]],
          y = 0.75*max(densities[[i]]$y),
          vjust = -0.1,
          fontface = "bold",
          size = 5
        )
      }
  }else if(!is.null(aes_color)) {
    aplot <- ggplot(data = df_copy, aes(x = !!rlang::sym(aes_x), color = !!rlang::sym(aes_color)))
    aplot <- aplot + stat_density(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      position = position,
      na.rm = na.rm,
      fill = density_fill,
      alpha = density_alpha,
      size = density_size
    )
  }else if(!is.null(aes_fill)) {
    aplot <- ggplot(data = df, aes(x = !!rlang::sym(aes_x), fill = !!rlang::sym(aes_fill)))
    aplot <- aplot + stat_density(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      position = position,
      na.rm = na.rm,
      size = density_size,
      color = density_color,
      alpha = density_alpha
    )
  }else {
    aplot <- ggplot(data = df, aes(x = !!rlang::sym(aes_x)))
    aplot <- aplot + stat_density(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      position = position,
      na.rm = na.rm,
      size = density_size,
      color = density_color,
      fill = density_fill,
      alpha = density_alpha
    )
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

  if(!is.null(palette_colors)){
    if(!is.null(aes_fill)) {
      aplot <- aplot + scale_fill_manual(values = palette_colors)
    }else if(!is.null(aes_color)) {
      aplot <- aplot + scale_color_manual(values = palette_colors)
    }
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

  aplot <- aplot + scale_x_continuous(
    limits = x_limits,
    breaks = x_major_breaks,
    minor_breaks = x_minor_breaks,
    labels = x_labels,
    expand = c(0,0)
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

  aplot <- aplot + labs(title = title, subtitle = subtitle)
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
