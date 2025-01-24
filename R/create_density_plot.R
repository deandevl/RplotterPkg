#' @title create_density_plot
#'
#' @description Function creates a ggplot2 based density plot with options for scaling, shading probability areas,
#'  and plotting observation locations. The function's density arguments mirror most of the arguments available
#'  from \code{\link{density}} for the Kernel Density Estimation (KDE). See the \code{\link{density}} help page
#'  for more information.
#'
#' @param df A required data frame that contains a numeric column from which to estimate the KDE along with possible variables
#'  for setting 'aes_color' and 'aes_fill' (see below).
#' @param aes_x A required string that sets the variable of 'df' from which to estimate the KDE.
#' @param aes_color Sets the variable name from 'df' for the aesthetic mapping for color.
#' @param aes_fill Sets the variable name from 'df' for the aesthetic mapping for fill.
#' @param bw  A string or numeric that sets the smoothing bandwidth to be used with the KDE function.
#' @param adjust A numeric that adjusts 'bw' since the actual bandwidth is computed as \code{adjust*bw}.
#' @param kernel A string that set the type of Kernel Density Estimation (KDE). Acceptable values are "gaussian",
#'  "rectangular", "triangular", "epanechnikov", "biweight", "cosine" or "optcosine".
#' @param n The number of equally spaced points at which the density is to be estimated. This should be a power of two.
#' @param position A string that sets the position.  Acceptable values are "identity" which overlays or
#'  "stack" which stacks.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the 'title' and 'subtitle'.
#' @param x_title A string that sets the x axis title. If \code{NULL} (the default)  then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If \code{NULL} (the default)  then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param density_linewdth A numeric that sets the width attribute for the density line .
#' @param density_color A string that sets the color attribute for the density line .
#' @param density_fill A string that sets the fill color attribute for the area under the density line.
#' @param density_alpha A numeric that sets the alpha attribute value for 'density_fill'.
#' @param x_limits A numeric 2 element vector or function that sets the minimum and maximum for the x axis. Use NA to
#'  refer to the existing minimum and maximum.
#' @param x_major_breaks A numeric vector or function that sets the major tic locations along the x axis.
#' @param x_minor_breaks A numeric vector or function that sets the minor tic locations along the x axis.
#' @param x_labels A character vector or function giving x axis tic labels.  Must be the same length as 'x_breaks'.
#' @param y_limits A numeric 2 element vector or function that sets the minimum and maximum for the y axis.  Use NA to
#'  refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that sets the major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that sets the minor tic locations along the y axis.
#' @param y_labels A character vector or function giving y axis tic labels.  Must be the same length as 'y_breaks'.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param plot_obs A logical which if \code{TRUE} plots a line for each observation along the axis margin.
#' @param plot_obs_len A numeric that sets the length of the 'plot_obs' lines.
#' @param plot_obs_color A string that sets the color of the 'plot_obs' lines.
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
#' @param cum_prob A one or two element numeric vector for defining cumulative probabilities which divide the density plot area. The
#'  values are probabilities with values within 0.0 to 1.0.  If for example with a cumulative probability 'cum_prob' of .95, then the density is divided
#'  into two probability areas of 95% and 5% with the dividing quantile value labeled. Similarly if two cumulative probabilities are submitted,
#'  then three areas are defined.
#' @param area_colors A string vector that sets the color of each area defined by 'cum_prob'.
#' @param area_quantile_line_color A string that sets the vertical line color at quantile locations that divide the areas defined
#'  by \code{cum_prob}.
#' @param png_file_path A character string with the directory and file name to produce
#'  a png image of the plot.
#' @param png_width_height A numeric vector that sets the width and height of the png image in pixels. The
#'  default is c(480,480).  There are 37.8 pixels in a centimeter.
#'
#' @return A ggplot class object.
#'
#' @examples
#' library(ggplot2)
#' library(rlang)
#' library(data.table)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_density_plot(
#'   df = datasets::airquality,
#'   aes_x = "Ozone",
#'   rot_y_tic_label = TRUE,
#'   x_limits = c(-40,200),
#'   x_major_breaks = seq(-40,200,20),
#'   title = "Ozone Air Quality",
#'   x_title = "Ozone",
#'   y_title = "Density",
#'   plot_obs = TRUE,
#'   density_fill = "green",
#'   density_alpha = 0.5
#' )
#'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @export
create_density_plot <- function(
    df = NULL,
    aes_x = NULL,
    aes_color = NULL,
    aes_fill = NULL,
    bw = "nrd0",
    adjust = 1,
    kernel = "gaussian",
    n = 512,
    position = "identity",
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
    density_linewdth = 1.0,
    density_color = "black",
    density_fill = "gray",
    density_alpha = 0.4,
    x_limits = NULL,
    x_major_breaks = waiver(),
    x_minor_breaks = waiver(),
    x_labels = waiver(),
    y_limits = NULL,
    y_major_breaks = waiver(),
    y_minor_breaks = waiver(),
    y_labels = waiver(),
    axis_text_size = 11,
    show_major_grids = TRUE,
    show_minor_grids = TRUE,
    plot_obs = FALSE,
    plot_obs_len = 0.02,
    plot_obs_color = "black",
    panel_color = "white",
    panel_border_color = "black",
    show_legend = TRUE,
    legend_pos = "right",
    legend_key_width = 0.7,
    legend_key_height = 0.7,
    legend_key_backgrd = "white",
    silent_NA_warning = FALSE,
    cum_prob = NULL,
    area_colors = c("gray", "green", "gray"),
    area_quantile_line_color = "red",
    png_file_path = NULL,
    png_width_height = c(480,480)){

  x <- y <- NULL

  if(!is.data.frame(df)){
    stop("df must be a dataframe")
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
    "density",
    list(
      fill = density_fill,
      color = density_color,
      linewidth = density_linewdth,
      alpha = density_alpha
    )
  )

  ggplot2::update_geom_defaults(
    "ribbon",
    list(
      fill = density_fill,
      color = NA,
      alpha = density_alpha
    )
  )

  # ---Convert df to a data.table
  dt <- data.table::as.data.table(df)

  # ------------Define the main ggplot2 plot object, aesthetics, geoms------------
  aplot <- ggplot()

  # ---Are we doing cumulative probabilties?-------------------------------
  if(!is.null(cum_prob)){
    a_density <- stats::density(
      x = dt[[aes_x]],
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      na.rm = TRUE
    )
    density_df <- data.table::data.table(
      x = a_density$x,
      y = a_density$y
    )
    if(length(cum_prob) == 1){
      quantiles <- stats::quantile(dt[[aes_x]], probs = cum_prob, na.rm = TRUE)
      subset_1 <- subset(density_df, x < quantiles[[1]])
      subset_2 <- subset(density_df, x >= quantiles[[1]])
      densities <- list(subset_1, subset_2)
      areas <- c(cum_prob, 1 - cum_prob)
    }else {
      quantiles <- stats::quantile(dt[[aes_x]], probs = cum_prob, na.rm = TRUE)
      subset_1 <- subset(density_df, x < quantiles[[1]])
      subset_2 <- subset(density_df, x >= quantiles[[1]] & x < quantiles[[2]])
      subset_3 <- subset(density_df, x >= quantiles[[2]])
      densities <- list(subset_1, subset_2, subset_3)
      areas <- round(c(cum_prob[[1]], cum_prob[[2]]-cum_prob[[1]], 1 - cum_prob[[2]]), digits = 3)
    }

    for(i in seq_along(seq_along(densities))){
      densities_n <- nrow(densities[[i]])

      # plot area
      aplot <- aplot + geom_ribbon(
        data = densities[[i]],
        aes(x = x, ymin = 0, ymax = y),
        fill = area_colors[[i]],
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
          linewidth = 1.4
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
  }else {
    aplot <- ggplot(
      data = dt,
      aes(
        x = !!sym(aes_x),
        color = !!aes_color,
        fill = !!aes_fill
      )
    ) +
    geom_density(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      position = position,
      na.rm = silent_NA_warning,
    )
    # Are we plotting observations below the x axis
    if(plot_obs){
      aplot <- aplot +
        geom_rug(
          aes(y = 0),
          position = "identity",
          color = plot_obs_color,
          length = grid::unit(plot_obs_len, "npc")
        )
    }
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

  # -------------------rotate/size tic labels--------------------
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

  # --------------------x/y axis titles------------------------
  if(is.null(x_title)) {
      aplot <- aplot +
        theme(
          axis.title.x = element_blank()
        )
    }else{
      aplot <- aplot +
        labs(x = x_title)
    }
    if(is.null(y_title)) {
      aplot <- aplot +
        theme(
          axis.title.y = element_blank()
        )
    }else{
      aplot <- aplot +
        labs(y = y_title)
    }

  # -------------------axis tic scaling------------------
  # ---X axis
  aplot <- aplot + scale_x_continuous(
    limits = x_limits,
    breaks = x_major_breaks,
    minor_breaks = x_minor_breaks,
    labels = x_labels,
    expand = c(0,0)
  )
  # ---Y axis
  aplot <- aplot + scale_y_continuous(
    limits = y_limits,
    breaks = y_major_breaks,
    minor_breaks = y_minor_breaks,
    labels = y_labels
  )

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
