#' @title create_density_ridge_plot
#'
#' @description Function creates ggplot2 based plots stacked vertically (also known as ridge or raincloud plots).
#'  The function's density arguments mirror most of the arguments available
#'  from \code{\link{density}} for the Kernel Density Estimation (KDE). See the \code{\link{density}} help page
#'  for more information.
#'
#' if \code{display_plot} is TRUE then the plots will be displayed. If \code{display_plot} is FALSE then
#' the function returns a class ggplot object which can be displayed from the console by entering
#' \code{grid::grid.draw(plot object)}
#'
#' @param df The source data frame from which the densities are plotted.
#' @param variables A vector that names the x axis variables from \code{df} for plotting their densities.
#' @param plot_heights A numeric that sets the plot height in centimeters for each variable in \code{variables}.
#'   The default is 3.5 centimeters in height for each plot.
#' @param plot_widths A numeric that sets the plot width in centimeters for each variable in \code{variables}.
#'   The default is 24 centimeters in width for each plot.
#' @param bw  A string or numeric that sets the smoothing bandwidth to be used with the KDE function.
#' @param adjust A numeric that adjusts \code{bw} since the actual bandwidth is computed as \code{adjust*bw}.
#' @param kernel A string that set the type of Kernel Density Estimation (KDE). Acceptable values are "gaussian",
#'  "rectangular", "triangular", "epanechnikov", "biweight", "cosine" or "optcosine".
#' @param n The number of equally spaced points at which the density is to be estimated. This should be a power of two.
#' @param na.rm A logical which if \code{TRUE}, missing values are removed from \code{df}. If \code{FALSE} any missing
#'  values cause an error.
#' @param title A string that sets the overall title.
#' @param title_fontsz A numeric that sets the title's font size. The default is 14.
#' @param x_title A string that sets the x axis title. If NULL (the default)  then the x axis title does not appear.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param density_linewdth A numeric that sets the density line width.
#' @param density_color A string that sets the color for the density line.
#' @param density_fill A string that sets the color for the density fill.
#' @param density_alpha A numeric that sets the alpha value for \code{density_fill}.
#' @param x_limits A numeric 2 element vector or function that sets the minimum and maximum for the x axis. Use NA to
#'  refer to the existing minimum and maximum.
#' @param x_major_breaks A numeric vector or function that sets the major tic locations along the x axis.
#' @param x_minor_breaks A numeric vector or function that sets the minor tic locations along the x axis.
#' @param x_labels A character vector or function giving x axis tic labels.  Must be the same length as \code{x_breaks}.
#' @param y_limits A numeric 2 element vector or function that sets the minimum and maximum for the y axis.  Use NA to
#'  refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that sets the major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that sets the minor tic locations along the y axis.
#' @param y_show_axis A logical which if TRUE will display the y-axis density for each variable.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param display_plot A logical that if TRUE will display the plot.
#'
#' @return A TableGrob class object.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_density_ridge_plot(
#'   df = RplotterPkg::midwest,
#'   bw = "sj",
#'   variables = c("HS_Diploma", "College_Edu", "Prof_Deg", "White", "Black", "Asian"),
#'   title = "Percent Distribution Among Midwest Counties",
#'   x_limits = c(0, 100),
#'   x_major_breaks = seq(0, 100, 10),
#'   density_fill = "blue",
#'   density_alpha = 0.5,
#'   display_plot = TRUE
#' )
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
#' @references Micah Allen,Davide Poggiali,Kirstie Whitaker,Tom Rhys Marshall,
#'  Jordy van Langen,Rogier A. Kievit (2021) Raincloud plots: a multi-platform tool for robust data visualization: version 2
#' @references Joachim Schork (2021) Ridgeline Plots in R (3 Examples)
#'
#' @export
create_density_ridge_plot <- function(
  df = NULL,
  variables = NULL,
  plot_heights = 3.5,
  plot_widths = 24,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  n = 512,
  na.rm = TRUE,
  title = NULL,
  title_fontsz = 14,
  x_title = NULL,
  rot_x_tic_angle = 0,
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
  y_show_axis = FALSE,
  axis_text_size = 11,
  display_plot = TRUE){

  if(!is.data.frame(df)){
    stop("df must be a dataframe")
  }

  if(is.null(variables)){
    stop("'variables' parameter must be defined.")
  }

  plot_fun <- function(aes_x, do_x_axis){
    aplot <- ggplot(data = df, aes(x = !!rlang::sym(aes_x)))

    aplot <- aplot + geom_density(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      na.rm = na.rm,
      linewidth = density_linewdth,
      color = density_color,
      fill = density_fill,
      alpha = density_alpha
    )

    aplot <- aplot +
      theme(
        panel.background = element_rect(fill = "white", color = "black")
      )

    aplot <- aplot + scale_x_continuous(
      limits = x_limits,
      breaks = x_major_breaks,
      minor_breaks = x_minor_breaks,
      labels = x_labels,
      expand = c(0,0)
    )

    if(y_show_axis){
      aplot <- aplot + scale_y_continuous(
        limits = y_limits,
        breaks = y_major_breaks,
        minor_breaks = y_minor_breaks,
      )
    }else {
      aplot <- aplot +
        theme (
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    }

    if(do_x_axis){
      if(rot_x_tic_angle > 0){
        aplot <- aplot +
          theme(axis.text.x = element_text(angle = rot_x_tic_angle, hjust = 1.0))
      }
      aplot <- aplot +
        theme(
          axis.text.x = element_text(size = axis_text_size, color = "black"),
          axis.title.x = element_text(size = axis_text_size + 2, color = "black")
        )
      aplot <- aplot +
        labs(x = x_title)
    }else {
      aplot <- aplot +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    }

    aplot <- aplot +
      labs(y = aes_x)

    aplot <- aplot +
      theme(
        panel.background = element_rect(color = NA),
        panel.grid = element_blank()
      )

    return(aplot)
  }

  plots <- vector(mode = "list", length = length(variables))

  for(i in seq_along(variables)){
    do_x_axis <- FALSE
    if(i == length(variables)){
      do_x_axis <- TRUE
    }

    plots[[i]] <- plot_fun(
      aes_x = variables[[i]],
      do_x_axis = do_x_axis
    )
  }
  row_heights <- c()
  title_grob <- NULL
  # Are we doing a title
  if(!is.null(title)){
    title_grob <- grid::textGrob(label = title, gp = grid::gpar(col = "black", fontsize = title_fontsz, fontface = 2L))
    row_heights <- c(1.0, row_heights)
  }

  # convert plots from ggplot to grobs
  plot_grobs <- vector(mode = "list", length = length(plots))
  for(i in seq_along(plots)){
    plot_grobs[[i]] <- ggplot2::ggplotGrob(plots[[i]])
  }

  # define row heights
  row_heights <- c(row_heights, rep(plot_heights, length(variables)))

  # define gtable
  plots_table <- gtable::gtable(
    name = "plots_table",
    widths = grid::unit(x = plot_widths, units = "cm"),
    heights = grid::unit(x = row_heights, units = "cm")
  )

  # for debug: show layout
  #gtable::gtable_show_layout(plots_table)

  idx <- 1
  # add title to table?
  if(!is.null(title_grob)){
    plots_table <- gtable::gtable_add_grob(
      x = plots_table,
      grobs = title_grob,
      t = 1,
      l = 1,
      r = 1
    )
    idx <- 2
  }
  # add the plots
  for(i in seq_along(variables)){
    plots_table <- gtable::gtable_add_grob(
      x = plots_table,
      grobs = plot_grobs[[i]],
      t = idx,
      l = 1,
      r = 1
     # b = idx + 1
    )
    idx <- idx + 1
  }

  if(display_plot){
    grid::grid.newpage()
    grid::grid.draw(plots_table)
  }
  return(plots_table)
}
