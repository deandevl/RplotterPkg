#' @title create_waffle_chart
#'
#' @description Function creates a "waffle" chart that displays percentages for multiple variables.
#' The chart is similar to a heatmap with a 10 by 10 array of tiles (each representing one percent) that
#' are colored to the extent of the variable's proportion value.
#' For more information on "waffle" charts see \href{https://baryon.be/uncommon-chart-types-waffle-charts/}{What is a waffle chart}.
#'
#' @param x Either a dataframe or a vector of named integer proportions.
#' @param name_col If 'x' is a dataframe, a required string setting the column name having the variable names.
#' @param prop_col If 'x' is a dataframe, a required string setting the column name having the variable proportions.
#' @param title A string that sets the main title for the chart.
#' @param select_fill A string that sets the fill color for tiles with proportion values.
#' @param default_fill A string that sets the fill color for tiles without proportion values.
#'
#' @return A ggplot2 class object
#'
#' @examples
#' library(grid)
#' library(gtable)
#' library(ggplotify)
#' library(ggplot2)
#' library(RplotterPkg)
#'
#' proportions_v <- c(
#' var1=10,var2=40,var3=20, var4=50, var5=5,
#' var6=30, var7=10, var8=67, var9=42, var10=33,
#' var11=7, var12=35,var13=22, var14=90
#' )
#' create_waffle_chart(
#'   x = proportions_v,
#'   title = "Test For 14 Proportion Variables"
#' )
#'
#' @import ggplot2
#' @import grid
#' @import gtable
#' @importFrom ggplotify as.ggplot
#'
#' @export
create_waffle_chart <- function(
    x = NULL,
    name_col = NULL,
    prop_col = NULL,
    title = NULL,
    select_fill = "brown",
    default_fill = "lightgray"
){

  if(is.null(x)){
    stop("The data(x parameter) is required")
  }

  rect_sz <- 4
  prop_names <- NULL
  prop_values <- NULL

  if(is.data.frame(x)){
    if(is.null(name_col) || is.null(prop_col)){
      stop("Both 'name_col' and 'prop_col' must be specified for a dataframe input.")
    }
    prop_names <- as.vector(x[[name_col]])
    prop_values <- as.vector(x[[prop_col]])
  }else if(is.vector(x)){
    prop_names <- names(x)
    prop_values <- x
  }else {
    stop("Only a named vector or datafram with columns for proportion variable names
         and values accepted.")
  }

  n_rows <- 1
  n_cols <- 5
  n_vars <- length(prop_values)
  n_cols_last <- 0
  if(n_vars < 6){
    n_cols <- n_vars
  }else{
    remain <- n_vars%%n_cols
    if(remain == 0){
      n_rows <- n_vars/n_cols
    }else{
      n_rows <- round(n_vars/n_cols, digits = 0) + 1
    }
  }

  make_tile_table <- function(a_name, a_prop){
    widths_tiles <- grid::unit(rep(rect_sz, 10), rep("mm",10))
    heights_tiles <- grid::unit(c(10, rep(rect_sz, 10)), c("mm", rep("mm",10)))
    tile_table <- gtable::gtable(
      name = paste0("tile_table ", a_prop),
      widths = widths_tiles,
      heights = heights_tiles
    )
    # debug
    # gtable::gtable_show_layout(tile_table, cell.label = FALSE)
    # browser()
    # add variable name textGrob
    tile_table <- gtable::gtable_add_grob(
      x = tile_table,
      grobs = grid::textGrob(
        label = a_name,
        gp = grid::gpar(col = "black", fontsize = 14, fontface = 2L)
      ),
      t = 1,
      l = 1,
      r = 10
    )
    # add 10 x 10 rectGrob's
    for(i in 1:10){
      for(ii in 1:10){
        rect_fill <- default_fill
        idx <- (i-1)*10 + ii
        if(idx > 100 - a_prop){
          rect_fill <- select_fill
        }
        rect_grob <- grid::rectGrob(
          gp = gpar(fill = rect_fill,  alpha = 1, col = "white", lwd = 1),
          width = unit((rect_sz),"mm"),
          height = unit((rect_sz),"mm"),
          default.units = "mm"
        )
        tile_table <- gtable::gtable_add_grob(
          x = tile_table,
          grobs = rect_grob,
          t = i + 1,
          l = ii
        )
      }
    }
    # debug
    # grid.newpage()
    # grid.draw(tile_table)
    # browser()
    return(tile_table)
  }

  # start creating main table
  tiles_table_w <- 10 * rect_sz + 2
  tiles_table_h <- 10 * rect_sz + 10
  main_table <- gtable::gtable(
    name = "main_table",
    widths = grid::unit(rep(tiles_table_w, n_cols), rep("mm",n_cols)),
    heights = grid::unit(c(14, rep(tiles_table_h, n_rows)), c("mm",rep("mm",n_rows)))
  )
  # debug
  # gtable::gtable_show_layout(main_table, cell.label = FALSE)
  # browser()
  # add title
  main_table <- gtable::gtable_add_grob(
    x = main_table,
    grobs = grid::textGrob(
      label = title,
      gp = grid::gpar(col = "black", fontsize = 20, fontface = 2L)
    ),
    t = 1,
    l = 1,
    r = n_cols
  )

  # add variable's tile gtable
  for(i in 1:n_rows){
    for(ii in 1:n_cols){
      idx <- (i - 1) * n_cols + ii
      if(idx > n_vars){
        a_plot <- ggplotify::as.ggplot(main_table)
        return(a_plot)
      }
      a_tile_table <- make_tile_table(
        a_name = prop_names[[idx]],
        a_prop = prop_values[[idx]]
      )
      main_table <- gtable::gtable_add_grob(
        x = main_table,
        grobs = a_tile_table,
        t = i + 1,
        l = ii
      )
    }
  }

  a_plot <- ggplotify::as.ggplot(main_table)
  return(a_plot)
}
