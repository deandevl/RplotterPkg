#' Arranges a group of ggplot2 plots or grid::grob's into multiple panels.
#'
#' Function creates a figure with title and arranges multiple plots/grobs
#'   across a given number of rows and columns. The function depends on the \code{grid},
#'   \code{gtable}, and \code{ggplotify} packages.
#'
#'  The function goes beyond just placing a group of plots across a grid of rows and columns.
#'  It is somewhat opinionated in favor of common titling/labeling and scaling
#'   that make sense for all the plots and provide the ability to compare. The function has a
#'   \code{title} parameter that is assumed appropriate for all the plots--any titles/subtitles
#'   among the plots themselves will be removed.  The first left y-axis
#'   scaling the function comes across will be used for all the plots. Similarly the first bottom
#'   x-axis scaling the function comes across will be assumed appropriate for all the plots. The
#'   first legend that is found is assumed right for all the plots and will be placed at the top
#'   in a horizontal direction. Of course none of these sub-elements among the plots need to exist
#'   and you are back to simply a grid of plots.
#'
#' As an example of defining the \code{layout} argument,
#'   \code{layout} has three named elements: "plots", "rows", and "cols".
#'   "plots" is a list() of ggplot2 plot objects; "rows" defines the row number
#'  for each plot and "cols" defines the column number for each plot. If we have
#'  two plots with both plots on a single row then "rows" = c(1,1) and "cols"
#'  = c(1,2).
#'
#' @param layout A named list containing a list for plot objects or grobs "plots", row locations "rows" vector,
#'  column locations "cols" vector. This argument is required.
#' @param title A string that sets the title of the figure.
#' @param title_fontsz A numeric that sets the title's font size. The default is 20.
#' @param plot_titles A character vector with the same length as the number of plot objects that defines
#'   each of their titles.
#' @param y_tick_width A numeric that sets the width of the vertical column containing the y axis tick labeling.
#'   The default is 0.5 cm and may be increased when a wider labeling is needed.
#' @param cell_width A numeric that sets the cell widths in the gtable in cm
#' @param cell_height A numeric that sets the cell widths in the gtable in cm
#' @param do_grid A logical which if TRUE will arrange the plot objects in a straight row/column order without considering
#'   their scale and label components.
#' @param display_plot A logical that if TRUE will display the plot, otherwise a ggplot2 object is returned.
#'
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom ggplotify as.ggplot
#' @import ggplot2
#'
#' @author Rick Dean
#'
#' @return A ggplot2 object if \code{display_plot} is FALSE.
#'
#' @export
multi_panel_grid <- function (
  layout,
  title = NULL,
  title_fontsz = 20,
  plot_titles = NULL,
  y_tick_width = 0.5,
  cell_width = 8,
  cell_height = 8,
  do_grid = FALSE,
  display_plot = TRUE) {

  title_grob <- NULL
  max_rows <- max(layout$rows)
  max_cols <- max(layout$cols)
  start_row <- 0
  start_col <- 0

  # Put all the original plots in a vector
  plots_v <- vector(mode = "list", length = length(layout[["plots"]]))
  for(i in seq_along(layout[["plots"]])){
    plots_v[[i]] <- layout[["plots"]][[i]]
  }

  # Common legend?--find any legends among "plots"
  legend_grob <- NULL
  if(!do_grid){
    for(i in seq_along(plots_v)){
      plots_v[[i]] <- plots_v[[i]] + theme(legend.direction = "horizontal")

      a_grob <- RplotterPkg::get_grob_component(
        a_plot = plots_v[[i]],
        component_name = "guide-box"
      )

      if(!is.null(a_grob) & is.null(legend_grob)) {
        legend_grob <- a_grob
        plots_v[[i]] <- plots_v[[i]] + theme(legend.position = "none")
      }
      plots_v[[i]] <- plots_v[[i]] + theme(legend.position = "none")
    }
  }

  # Are we doing a title?
  title_grob <- NULL
  if(!is.null(title)){
    title_grob <- grid::textGrob(label = title, gp = grid::gpar(col = "black", fontsize = title_fontsz, fontface = 2L))
  }

  # Any x axis bottom titles among "plots_v"
  xtitle_grob <- NULL
  if(!do_grid){
    for(i in seq_along(plots_v)){
      a_grob <- RplotterPkg::get_grob_component(
        a_plot = plots_v[[i]],
        component_name = "xlab-b"
      )
      if(!is.null(a_grob) & is.null(xtitle_grob)) {
        xtitle_grob <- a_grob
      }
      plots_v[[i]] <- plots_v[[i]] + theme(
        axis.title.x = element_blank()
      )
    }
  }

  # Any bottom x axis among "plots_v"
  xaxis_grob <- NULL
  if(!do_grid){
    for(i in seq_along(plots_v)){
      a_grob <- RplotterPkg::get_grob_component(
        a_plot = plots_v[[i]],
        component_name = "axis-b"
      )
      if(!is.null(a_grob) & is.null(xaxis_grob)) {
        xaxis_grob <- a_grob
      }
      plots_v[[i]] <- plots_v[[i]] + theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
      )
    }
  }

  # Any left y axis among "plots_v"
  yaxis_grob <- NULL
  if(!do_grid){
    for(i in seq_along(plots_v)){
      a_grob <- RplotterPkg::get_grob_component(
        a_plot = plots_v[[i]],
        component_name = "axis-l"
      )
      if(!is.null(a_grob) & is.null(yaxis_grob)) {
        yaxis_grob <- a_grob
      }
      plots_v[[i]] <- plots_v[[i]] + theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      )
    }
  }

  # Any y axis titles among "plots_v"
  ytitle_grob <- NULL
  if(!do_grid){
    for(i in seq_along(plots_v)){
      a_grob <- RplotterPkg::get_grob_component(
        a_plot = plots_v[[i]],
        component_name = "ylab-l"
      )
      if(!is.null(a_grob) & is.null(ytitle_grob)) {
        ytitle_grob <- a_grob
      }
      plots_v[[i]] <- plots_v[[i]] + theme(
        axis.title.y = element_blank()
      )
    }
  }

  # Get all of the "panel" grobs
  panels_grob_v <- vector(mode = "list", length = length(layout[["plots"]]))
  if(!do_grid){
    for(i in seq_along(plots_v)){
      a_grob <- RplotterPkg::get_grob_component(
        a_plot = plots_v[[i]],
        component_name = "panel"
      )
      if(!is.null(a_grob)) {
        panels_grob_v[[i]] <- a_grob
      }else {
        stop(paste0("Could not locate the 'panel' component from plot ",i))
      }
    }
  }else {
    for(i in seq_along(plots_v)){
      panels_grob_v[[i]] <- ggplotify::as.grob(plots_v[[i]])
    }
  }

  #Create a gtable
  plots_table <- NULL

  # Calculate grid heights
  # From the top: title, legend, plot titles, plots, x-axis, x-axis title
  heights <- NULL
  if(!do_grid){
    if(is.null(legend_grob) & is.null(title_grob)){
      # only (plot titles,plots),x-axis, x-axis title
      heights <- grid::unit(c(rep(c(1,cell_height),max_rows),0.5,0.7), c(rep(c("cm","cm"),max_rows),"cm","cm"))
      start_row <- 0
    }else if(is.null(legend_grob) & !is.null(title_grob)){
      # only title,(plot titles,plots),x-axis, x-axis title
      heights <- grid::unit(c(1,rep(c(1,cell_height),max_rows),0.5,0.7), c("cm",rep(c("cm","cm"),max_rows),"cm","cm"))
      start_row <- 1
    }else if(!is.null(legend_grob) & is.null(title_grob)){
      # only legend,(plot titles,plots),x-axis, x-axis title
      heights <- grid::unit(c(1,rep(c(1,cell_height),max_rows),0.5,0.7), c("cm",rep(c("cm","cm"),max_rows),"cm","cm"))
      start_row <- 1
    }else if(!is.null(legend_grob) & !is.null(title_grob)){
      # the whole thing: title,legend,(plot titles,plots),x-axis, x-axis title
      heights <- grid::unit(c(1,0.5,1,rep(c(1,cell_height),max_rows),0.5,0.7), c("cm","cm","cm",rep(c("cm","cm"),max_rows),"cm","cm"))
      start_row <- 3
    }
  }else{
    heights <- grid::unit(rep(cell_height, max_rows), rep("cm", max_rows))
  }

  # Calculate grid widths
  # From the left: ytitle,yaxis,spacers,plots
  widths <- NULL
  if(!do_grid){
    if(is.null(ytitle_grob) & is.null(yaxis_grob)){
      # only spacers,plots
      widths <- grid::unit(rep(c(cell_width,0.2),max_cols),rep(c("cm","cm"),max_cols))
      start_col <- 0
    }else if(is.null(ytitle_grob) & !is.null(yaxis_grob)){
      # only yaxis,spacers,plots
      widths <- grid::unit(c(y_tick_width,rep(c(cell_width,0.2),max_cols)),c("cm",rep(c("cm","cm"),max_cols)))
      start_col <- 1
    }else if(!is.null(ytitle_grob) & is.null(yaxis_grob)){
      # only ytitle,spacers,plots
      widths <- grid::unit(c(0.7,rep(c(cell_width,0.2),max_cols)),c("cm",rep(c("cm","cm"),max_cols)))
      start_col <- 1
    }else if(!is.null(ytitle_grob) & !is.null(yaxis_grob)){
      # the whole thing: ytitle,yaxis,spacers,plots
      widths <- grid::unit(c(0.7,y_tick_width,rep(c(cell_width,0.2),max_cols)),c("cm","cm",rep(c("cm","cm"),max_cols)))
      start_col <- 2
    }
  }else {
    widths <- grid::unit(rep(c(0.5,cell_width), max_cols), rep(c("cm", "cm"), max_cols))
  }

  # define gtable
  plots_table <- gtable::gtable(
    name = "plots_table",
    widths = widths,
    heights = heights
  )

  # for debug: show layout
  #gtable::gtable_show_layout(plots_table)

  # Add row based grobs to grid table (title, legend, x axis)
  if(!do_grid){
    if(is.null(legend_grob) & !is.null(title_grob)){
      # add only title to table
      plots_table <- gtable::gtable_add_grob(
        x = plots_table,
        grobs = title_grob,
        t = 1,
        l = 1,
        r = start_col + 2*max_cols
      )
    }else if(!is.null(legend_grob) & is.null(title_grob)){
      # add only legend to table
      plots_table <- gtable::gtable_add_grob(
        x = plots_table,
        grobs = legend_grob,
        t = 1,
        l = 1,
        r = start_col + 2*max_cols
      )
    }else if(!is.null(legend_grob) & !is.null(title_grob)){
      # add both title and legend to table
      plots_table <- gtable::gtable_add_grob(
        x = plots_table,
        grobs = title_grob,
        t = 1,
        l = 1,
        r = start_col + 2*max_cols
      )
      plots_table <- gtable::gtable_add_grob(
        x = plots_table,
        grobs = legend_grob,
        t = 3,
        l = 1,
        r = start_col + 2*max_cols
      )
    }
  }
  # Add x-axis
  if(!do_grid){
    if(!is.null(xaxis_grob)){
      for(i in 1:max_cols){
        plots_table <- gtable::gtable_add_grob(
          x = plots_table,
          grobs = xaxis_grob,
          t = start_row + max_rows*2 + 1,
          l = start_col + i * 2 - 1
        )
      }
    }
  }

  # Add x-axis title
  if(!do_grid){
    if(!is.null(xtitle_grob)){
      for(i in 1:max_cols){
        plots_table <- gtable::gtable_add_grob(
          x = plots_table,
          grobs = xtitle_grob,
          t = start_row + max_rows*2 + 2,
          l = start_col + i * 2 - 1
        )
      }
    }
  }

  # Add column based grobs to grid table (y-axis title, y-axis ticks)
  if(!do_grid){
    if(is.null(ytitle_grob) & !is.null(yaxis_grob)){
      for(i in 1:max_rows){
        plots_table <- gtable::gtable_add_grob(
          x = plots_table,
          grobs = yaxis_grob,
          t = start_row + i * 2,
          l = 1
        )
      }
    }else if(!is.null(ytitle_grob) & is.null(yaxis_grob)){
      for(i in 1:max_rows){
        plots_table <- gtable::gtable_add_grob(
          x = plots_table,
          grobs = ytitle_grob,
          t = start_row + i * 2,
          l = 1
        )
      }
    }else if(!is.null(ytitle_grob) & !is.null(yaxis_grob)){
      for(i in 1:max_rows){
        plots_table <- gtable::gtable_add_grob(
          x = plots_table,
          grobs = ytitle_grob,
          t = start_row + i * 2,
          l = 1
        )
      }
      for(i in 1:max_rows){
        plots_table <- gtable::gtable_add_grob(
          x = plots_table,
          grobs = yaxis_grob,
          t = start_row + i * 2,
          l = 2
        )
      }
    }
  }

  # Add plot titles
  if(!do_grid){
    if(!is.null(plot_titles)){
      for(i in 1:max_rows){
        for(ii in 1:max_cols){
          a_title <- plot_titles[[(i-1)*max_cols + ii]]
          plot_title_grob <- grid::textGrob(label = a_title, gp = grid::gpar(col = "black", fontsize = 14, fontface = 2L))
          plots_table <- gtable::gtable_add_grob(
            x = plots_table,
            grobs = plot_title_grob,
            t = start_row + i * 2 - 1,
            l = start_col + ii * 2 - 1
          )
        }
      }
    }
  }

  # Add panels
  if(!do_grid){
    for(i in 1:max_rows){
      for(ii in 1:max_cols){
        plots_table <- gtable::gtable_add_grob(
          x = plots_table,
          grobs = panels_grob_v[[(i-1)*max_cols + ii]],
          t = start_row + i * 2,
          l = start_col + ii * 2 - 1
        )
      }
    }
  }else{
    for(i in 1:length(panels_grob_v)){
      plots_table <- gtable::gtable_add_grob(
        x = plots_table,
        grobs = panels_grob_v[[i]],
        t = layout$rows[[i]],
        l = layout$cols[[i]] * 2
      )
    }
  }

  # Display plot table?
  a_plot <- ggplotify::as.ggplot(plots_table)
  if(display_plot){
    a_plot
  }else{
    return(a_plot)
  }
}

