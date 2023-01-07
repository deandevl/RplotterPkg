library(grid)
library(gtable)

#' Arranges a group of ggplot2 plots or grid::grobs into multiple panels.
#'
#' Function creates a figure with title/subtitle and arranges multiple plots/grobs
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
#' @param col_widths A numeric vector of column widths in inches. Note that if there are two
#'  columns 4 inches wide then the argument should be \code{c(4,4)}.
#' @param row_heights A numeric vector of row heights in inches. Note that if there are two
#'  rows 5 inches high then the argument should be \code{c(5,5)}. If we have one row 5 inches high then
#'  this argument should just be assigned the value 5.
#' @param title A string that sets the title of the figure.
#' @param subtitle A string that sets the subtitle of the figure.
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
  col_widths = c(4,4),
  row_heights = c(4,4),
  title = NULL,
  subtitle = NULL,
  display_plot = TRUE) {

  titles <- list()
  # Are we doing a title
  if(!is.null(title)){
    titles[["a_title"]] <- grid::textGrob(label = title, gp = grid::gpar(col = "black", fontsize = 18, fontface = 2L))
  }

  # Are we doing a subtitle
  if(!is.null(subtitle)){
    titles[["a_subtitle"]] <- grid::textGrob(label = subtitle, gp = grid::gpar(col = "black", fontsize = 14))
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

  # define row heights
  heights_v <- c(rep(1.2, length(titles)), row_heights)
  heights_units <- rep("in",length(row_heights))
  heights_units <- c(rep("cm", length(titles)),heights_units)

  # define gtable
  plots_table <- gtable::gtable(
    name = "plots_table",
    widths = grid::unit(x = col_widths, units = "in"),
    heights = grid::unit(heights_v, units = heights_units)
  )

  # for debug: show layout
  #gtable::gtable_show_layout(plots_table)

  # find the max column index
  max_col <- 0
  for(col in layout[["cols"]]){
    if(max(col) > max_col){
      max_col <-  max(col)
    }
  }

  # add titles to table
  idx <- 1
  for(item in titles){
    plots_table <- gtable::gtable_add_grob(
      x = plots_table,
      grobs = item,
      t = idx,
      l = 1,
      r = max_col
    )
    idx <- idx + 1
  }

  for(i in seq_along(layout[["plots"]])){
    plots_table <- gtable::gtable_add_grob(
      x = plots_table,
      grobs = plot_grobs[[i]],
      t = min(layout[["rows"]][[i]]) + length(titles),
      l = min(layout[["cols"]][[i]]),
      r = max(layout[["cols"]][[i]]),
      b = max(layout[["rows"]][[i]]) + length(titles)
    )
  }

  if(display_plot){
    grid::grid.newpage()
    grid::grid.draw(plots_table)
  }else{
    return(plots_table)
  }
}

