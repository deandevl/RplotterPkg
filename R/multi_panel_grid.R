library(grid)
library(gtable)

#' Arranges a group of ggplot2 plots or grid::grobs into multiple panels.
#'
#' Function creates a figure with title, subtitle, legend and arranges multiple plots
#' across a given number of rows and columns. The function depends on the \code{grid} and \code{gtable} packages.
#'
#' @param layout A named list containing a list for plot objects or grobs \dQuote{plots}, row locations \dQuote{rows} vector,
#'  column locations \dQuote{cols} vector.
#' @param col_widths A numeric vector of column widths in inches.
#' @param row_heights A numeric vector of row heights in inches.
#' @param title A string that sets the title of the figure.
#' @param subtitle A string that sets the subtitle of the figure.
#' @param do_legend A logical that displays a common legend among the plots.
#' @param display_plot A logical that if TRUE will display the plot
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
#' @return A grob object. Note that if \code{display_plot} is set to \code{FALSE}, then to produce graphical output you must use
#'  \code{grid::grid.draw()} on the returned grob object.
#'
#' @author Rick Dean
#'
#' @export
multi_panel_grid <- function (
  layout = NULL,
  col_widths = c(4,4),
  row_heights = c(4,4),
  title = NULL,
  subtitle = NULL,
  do_legend = FALSE,
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

  # Are we doing a legend that is assumed to be common among all the plots
  if(do_legend){
    plot_grob <- ggplot2::ggplotGrob(layout$plots[[1]])
    grobs <- plot_grob$grobs
    idx <- which(sapply(grobs, function(x) x$name) == "guide-box")
    if(!is.null(idx)) {
      titles[["a_legend"]] <- grobs[[idx]]

      # set legend.position to "none" for all the plot objects
      for(i in seq_along(layout$plots)){
        if(is.ggplot(layout$plots[[i]])){
          layout$plots[[i]] <- layout$plots[[i]] + theme(legend.position = "none")
        }
      }
    }
  }

  # convert plots from ggplot to grobs
  plot_grobs <- list()
  for(i in seq(1, length(layout$plots), by=1)){
    if(is.ggplot(layout$plots[[i]])){
      plot_grobs[[i]] <- ggplot2::ggplotGrob(layout$plots[[i]])
    }else {
      plot_grobs[[i]] <- layout$plots[[i]]
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

  # find the max column index
  max_col = 0
  for(col in layout$cols){
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

  for(i in seq(1, length(layout$plots), by=1)){
    plots_table <- gtable::gtable_add_grob(
      x = plots_table,
      grobs = plot_grobs[[i]],
      t = min(layout$rows[[i]]) + length(titles),
      l = min(layout$cols[[i]]),
      r = max(layout$cols[[i]]),
      b = max(layout$rows[[i]]) + length(titles)
    )
  }

  if(display_plot){
    grid::grid.newpage()
    grid::grid.draw(plots_table)
  }

  return(plots_table)
}

