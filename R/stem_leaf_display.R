#' Function is a wrapper around aplpack::stem.leaf that provides one or more stem and leaf display(s).
#'
#' Function accepts a named list of numeric vectors from which stem and leaf displays
#'   are provided.
#'
#' @param x The named list of numeric vectors from which stem and leaf displays are provided.
#' @param unit Leaf unit, as a power of 10 (e.g. 100, 0.01). The default is 1.
#' @param m Number of parts (1, 2, 5) into which each stem will be separated. The default is 1.
#' @param min_val Optional numeric that sets the smallest non-outlying value.
#' @param max_val Optional numeric that sets the largest non-outlying value.
#' @param outliers A logical which if TRUE (the default), outliers are placed on LO and HI stems
#' @param depths A logical which if TRUE (the default), print a column of "depths" to the left of the stems
#' @param col_width A numeric that sets the display column widths in cm. The default is 4, which
#'   works when \code{depths} is FALSE. You may need to increase this value to avoid cutting off long leaves.
#' @param font_sz A numeric that sets the display's font size. The default is 11.
#' @param heading_color A string that sets the heading's color in name or hex. The default is "black".
#' @param display_grob A logical that if TRUE (the default) will display the TableGrob.
#'
#' @importFrom aplpack stem.leaf
#'
#' @author Rick Dean
#'
#' @return A TableGrob object if \code{display_grob} is FALSE.
#'
#' @export
stem_leaf_display <- function(
    x,
    unit = 1,
    m = 1,
    min_val = NULL,
    max_val = NULL,
    outliers = TRUE,
    depths = FALSE,
    col_width = 4,
    font_sz = 11,
    heading_color = "black",
    display_grob = TRUE
) {

  var_names <- names(x)
  values <- unlist(x)

  if(is.null(min_val)){
    min_val <- min(values)
  }
  if(is.null(max_val)){
    max_val <- max(values)
  }

  stem_leaf_lst <- aplpack::stem.leaf(
    data = values,
    unit = unit,
    m = m,
    Min = min_val,
    Max = max_val,
    trim.outliers = outliers,
    depths = depths,
    printresult = F
  )

  col_widths <- rep(col_width, length(var_names) + 1)
  row_heights <- rep(0.5, length(stem_leaf_lst$stem) + 2) # adds 2 lines for info and textGrob headings
  display_table <- gtable::gtable(
    name = "display_table",
    widths = grid::unit(x = col_widths, units = "cm"),
    heights = grid::unit(x = row_heights, units = "cm")
  )
  # for debug: show layout
  #gtable::gtable_show_layout(display_table)

  # creating info textGrob
  info_grob <- grid::textGrob(
    label = paste(stem_leaf_lst$info[[1]], stem_leaf_lst$info[[2]], stem_leaf_lst$info[[3]], sep = "  "),
    just = "center",
    gp = grid::gpar(col = "black", fontsize = 12, fontface = 2L)
  )

  # creating heading textGrobs
  heading_grobs <- vector(mode = "list", length = length(var_names))
  for(i in seq_along(var_names)){
    heading_grobs[[i]] <- grid::textGrob(
      label = var_names[[i]],
      just = "left",
      gp = grid::gpar(col = heading_color, fontsize = 12, fontface = 2L))
  }

  # create stem & leaf textGrobs
  n_rows <- length(stem_leaf_lst$stem)
  n_cols <- length(var_names)
  n <- n_rows * n_cols
  stem_leaf_grobs <- vector(mode = "list", length = n)

  for(i in seq_along(var_names)){
    var_stem_leaf_lst <- aplpack::stem.leaf(
      data = x[[var_names[[i]]]],
      unit = unit,
      m = m,
      Min = min_val,
      Max = max_val,
      trim.outliers = outliers,
      depths = depths,
      printresult = F
    )

    for(ii in seq_along(var_stem_leaf_lst$stem)){
      display_str <- ""
      if(depths){
        display_str <- var_stem_leaf_lst$depths[[ii]]
      }
      display_str <- paste0(display_str, var_stem_leaf_lst$stem[[ii]], var_stem_leaf_lst$leaves[[ii]])
      stem_leaf_grobs[[(i - 1) * n_rows + ii]] <- grid::textGrob(
        label = display_str,
        just = "left",
        gp = grid::gpar(col = "black", fontsize = font_sz, fontface = 2L))
    }
  }

  # add info textGrob to gtable
  display_table <- gtable::gtable_add_grob(
    x = display_table,
    grobs = info_grob,
    t = 1,
    l = 1,
    r = length(var_names)+1
  )

  # add heading textGrobs to gtable
  for(i in seq_along(var_names)){
    display_table <- gtable::gtable_add_grob(
      x = display_table,
      grobs = heading_grobs[[i]],
      t = 2,
      l = i,
      r = i + 1
    )
  }

  # add stem & leaf textGrobs to gtable
  for(i in seq_along(var_names)){
    for(ii in seq_along(var_stem_leaf_lst$stem)){
      display_table <- gtable::gtable_add_grob(
        x = display_table,
        grobs = stem_leaf_grobs[[(i - 1) * n_rows + ii]],
        t = ii + 2,  # first two lines are info and the heading textGrobs
        l = i,
        r = i + 1
      )
    }
  }

  if(display_grob){
    grid::grid.newpage()
    grid::grid.draw(display_table)
  }else{
    return(display_table)
  }
}

