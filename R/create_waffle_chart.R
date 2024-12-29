#' @title create_waffle_chart
#' 
#' @description Function creates a "waffle" chart that displays percentages for multiple categorical variables.
#'   For more information on "waffle" charts see \href{https://baryon.be/uncommon-chart-types-waffle-charts/}{What is a waffle chart}.
#'
#' @param x Either a dataframe or a vector of named integer proportions.
#' @param name_col If *x* is a dataframe, the column name having the variable names.
#' @param prop_col If *x* is a dataframe, the column name having the variable proportions.
#' @param title A main title for the chart.
#' @param background_color A string that sets the background color.
#' @param cell_fill A string that sets the cell fill color.
#' @param cell_color A string that sets the cell boundary line color.
#' @param cell_nonfill A string that sets the non-cell fill color.
#' @param cell_alpha A numeric that sets the cell's fill alpha.
#' @param line_width A numeric that sets the line width among the cells
#'
#' @return A ggplot class object
#' 
#' @examples
#' library(grid)
#' library(RplotterPkg)
#' 
#' proportions_v <- c(
#'   var1=10, var2=40, var3=20, var4=50, var5=5,
#'   var6=30, var7=10, var8=67, var9=42, var10=33,
#'   var11=7, var12=35, var13=22, var14=90, var15=43
#' )
#' 
#' RplotterPkg::create_waffle_chart(
#'   x = proportions_v,
#'   title = "Test For 15 Proportion Variables",
#'   cell_color = "white",
#'   cell_fill = "brown",
#'   cell_nonfill = "lightgray",
#'   cell_alpha = 0.5,
#'   line_width = 4
#' )
#' 
#' @import grid
#' 
#' @export
create_waffle_chart <- function(
  x = NULL,
  name_col = NULL,
  prop_col = NULL,
  title = NULL,
  background_color = "yellow",
  cell_fill = "purple",
  cell_color = "black",
  cell_nonfill = "white",
  cell_alpha = 1,
  line_width = 1
){

  if(is.null(x)){
    stop("The data(x parameter) is required")
  }

  prop_names <- NULL
  prop_values <- NULL

  if(is.data.frame(x)){
    if(is.null(name_col) || is.null(prop_col)){
      stop("Both 'name_col' and 'prop_col' must be specified for a dataframe input.")
    }
    prop_names <- x[name_col]
    prop_values <- x[prop_col]
  }else {
    prop_names <- names(x)
    prop_values <- x
  }

  cell_w_h <- 4
  n_rows <- 1
  n_cols <- 6
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
      n_cols_last <- remain
    }
  }
  ymax <- n_rows * 55

  grid::grid.newpage()

  grid::grid.text(title,
    x = unit(5, "cm"),
    y = unit(1, "npc") - unit(1, "line"),
    just = c("left", "top"),
    gp = gpar(fontsize = 24, fontface = "bold")
  )

  grid::pushViewport(grid::viewport(
    x = unit(10, "mm"),
    y = unit(1, "npc") - unit(6, "line"),
    width = unit(275, "mm"),
    height = unit(ymax, "mm"),
    just = c("left", "top"),
  ))

  grid::grid.rect(gp = gpar(fill = background_color))

  # begin charting
  for(irow in 1:n_rows){
    for(icol in 1:n_cols){
      x_loc <- (icol-1)*45+5
      y_loc <- (ymax - 55) - (irow-1)*55
      idx <- (irow - 1) * n_cols + icol
      if(idx <= length(prop_values)){
        proportion <- prop_values[idx]

        for(j in 1:10){
          for(k in 1:10){
            x = unit(x_loc + k*cell_w_h - 2.0, "mm")
            y = unit(y_loc + j*cell_w_h - 2.0, "mm")
            vp <- grid::viewport(
              x = x,
              y = y,
              width = unit(cell_w_h, "mm"),
              height = unit(cell_w_h, "mm")
            )
            grid::pushViewport(vp)
            cell_id <- (j-1) * 10 + k
            if(cell_id <= proportion){
              grid::grid.roundrect(
                gp = gpar(
                  fill=cell_fill,
                  col=cell_color,
                  alpha=cell_alpha,
                  linejoin="round",
                  lwd = line_width
                ))
            }else{
              grid::grid.roundrect(
                gp = gpar(
                  fill=cell_nonfill,
                  col=cell_color,
                  lwd = line_width
                ))
            }
            grid::popViewport()
          }
        }
        grid::grid.text(prop_names[idx], x = unit(x_loc, "mm"), y = unit(y_loc+44, "mm"))
      }
    }
  }
  grid::popViewport()
}
