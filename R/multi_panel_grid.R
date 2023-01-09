library(grid)
library(gtable)

#' Arranges a group of ggplot2 plots or grid::grobs into multiple panels.
#'
#' Function creates a figure with title and arranges multiple plots/grobs
#'   across a given number of rows and columns. The function depends on the \code{grid}
#'   and \code{gtable} packages. As an example of defining the \code{layout} argument,
#'   \code{layout} has three named elements: "plots", "rows", and "cols".
#'   "plots" is a list() of ggplot2 plot objects; "rows" defines the row number
#'  for each plot and "cols" defines the column number for each plot. If we have
#'  two plots with both plots on a single row then "rows" = c(1,1) and "cols"
#'  = c(1,2).
#'
#' if \code{display_plot} is TRUE then the plots will be displayed. If \code{display_plot} is FALSE then
#' the function returns a plot object which can be displayed from the console by entering
#' \code{grid::grid.draw(plot object)}
#'
#' @param layout A named list containing a list for plot objects or grobs "plots", row locations "rows" vector,
#'  column locations "cols" vector. This argument is required.
#' @param col_widths A numeric vector of each column's widths in centimeters. Note that if there are two
#'  columns 8 centimeters wide then the argument should be \code{c(8,8)}.
#' @param row_heights A numeric vector of each row's heights in centimeters. Note that if there are two
#'  rows 10 centimeters high then the argument should be \code{c(10,10)}. If we have one row 10 centimeters
#'  high then this argument should just be assigned the value 10.
#' @param title A string that sets the title of the figure.
#' @param title_fontsz A numeric that sets the title's font size. The default is 14.
#' @param display_plot A logical that if TRUE will display the plot.
#'
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @import ggplot2
#'
#' @author Rick Dean
#'
#' @return A TableGrob object if \code{display_plot} is FALSE.
#'
#' @export
multi_panel_grid <- function (
  layout,
  col_widths = c(8,8),
  row_heights = c(8,8),
  title = NULL,
  title_fontsz = 14,
  display_plot = TRUE) {

  title_grob <- NULL
  # Are we doing a title
  if(!is.null(title)){
    title_grob <- grid::textGrob(label = title, gp = grid::gpar(col = "black", fontsize = title_fontsz, fontface = 2L))
    row_heights <- c(1.0, row_heights)
  }

  # convert plots from ggplot to grobs
  plot_grobs <- vector(mode = "list", length = length(layout[["plots"]]))
  for(i in seq_along(layout[["plots"]])){
    if(is.ggplot(layout[["plots"]][[i]])){
      plot_grobs[[i]] <-  ggplot2::ggplotGrob(layout[["plots"]][[i]])
    }else {
      plot_grobs[[i]] <- layout[["plots"]][[i]]
    }
  }

  # define gtable
  plots_table <- gtable::gtable(
    name = "plots_table",
    widths = grid::unit(x = col_widths, units = "cm"),
    heights = grid::unit(x = row_heights, units = "cm")
  )

  # for debug: show layout
  gtable::gtable_show_layout(plots_table)
  browser()
  idx <- 0
  # add title to table?
  if(!is.null(title_grob)){
    idx <- 1
    # find the max column index
    max_col <- 0
    for(col in layout[["cols"]]){
      if(max(col) > max_col){
        max_col <-  max(col)
      }
    }
    # add title to table
    plots_table <- gtable::gtable_add_grob(
      x = plots_table,
      grobs = title_grob,
      t = 1,
      l = 1,
      r = max_col
    )
  }
  # add the plots/grobs
  for(i in seq_along(layout[["plots"]])){
    plots_table <- gtable::gtable_add_grob(
      x = plots_table,
      grobs = plot_grobs[[i]],
      t = min(layout[["rows"]][[i]]) + idx,
      l = min(layout[["cols"]][[i]]),
      r = max(layout[["cols"]][[i]]),
      b = max(layout[["rows"]][[i]]) + idx
    )
  }

  if(display_plot){
    grid::grid.newpage()
    grid::grid.draw(plots_table)
  }else{
    return(plots_table)
  }
}

