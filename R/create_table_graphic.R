#' @title create_table_graphic
#'
#' @description Function creates a ggplot2 graphic table from a data frame.
#'  Function creates a simple, non-scrollable static table to be laid out with other ggplot2 graphics.  The
#'  function is based on \code{grid} along with the \code{gtable} package.
#'
#'  For estimating the overall height of the table consider that each row of the table is 0.2 inches high, .54 inches for heading and
#'  if a title is defined then add an additional 0.5 inches.
#'
#' @param df The target data frame from which to graph.
#' @param table_width The overall table width in inches.
#' @param head_color Color of the headings foreground.
#' @param head_fill Color of the headings background.
#' @param head_font_sz Font size of the headings.
#' @param cell_color Color of the cells foreground.
#' @param cell_fill Color of the cells background.
#' @param cell_font_sz Font size of the cells.
#' @param cell_just An string that sets justification of the cell content. Acceptable values are \dQuote{left} and \dQuote{right} and \dQuote{center}.
#' @param cell_hor_pos A numeric that sets the starting horizontal location of text in a cell. Acceptable range from 0.0 to 1.0.
#' @param title A string that sets the table title.
#' @param title_font_sz Font size of the title.
#' @param show_row_names A logical that controls the appearance of row names in the data frame.
#' @param row_names_width The width of the column for row names in inches.
#'
#' @return A ggplot class object.
#'
#' @examples
#' library(grid)
#' library(gtable)
#' library(ggplotify)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_table_graphic(
#'   df = RplotterPkg::car_stats,
#'   table_width = 7,
#'   show_row_names = TRUE,
#'   cell_just = "right",
#'   cell_hor_pos = 0.95,
#'   title = "Average Measures by Cylinders"
#' )
#'
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom grid textGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid grid.draw
#' @importFrom grid grobTree
#' @importFrom ggplotify as.ggplot
#'
#' @export
create_table_graphic <- function(
  df = NULL,
  table_width = 8,  # in "inches"
  head_color = "black",
  head_fill = "white",
  head_font_sz = 14,
  cell_color = "black",
  cell_fill = "white",
  cell_font_sz = 11,
  cell_just = "center",
  cell_hor_pos = 0.5,
  title = NULL,
  title_font_sz = 16,
  show_row_names = TRUE,
  row_names_width = 0.5 # in "inches"
){

  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
  }

  # convert df to a character data frame
  df_char <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

  # -----------------------define parameters for graphical table (width is in "cm"; height is in "cm") -------------------
  cell_width <- table_width/ncol(df_char)
  col_widths <- rep(cell_width, ncol(df_char))

  if(show_row_names){
    row_names <- rownames(df)
    df_char <- cbind(row_names, df_char)
    colnames(df_char)[1] <- " "
    col_widths <- c(row_names_width, col_widths) # the first column is for row numbers (row_names_width inches wide)
  }
  # .5 inches for title, .5 inches for headings; .2 inches for segment line; .2 inches for each row of df_char
  if(!is.null(title)){
    row_heights <- c(0.5, 0.5, 0.04, rep(0.2, nrow(df_char)))
  }else {
    row_heights <- c(0.5, 0.04, rep(0.2, nrow(df_char)))
  }


  # create gtable
  graphic_table <- gtable::gtable(
    name = "graphic_table",
    widths = grid::unit(x = col_widths, units = "in"),
    heights = grid::unit(x = row_heights, units = "in")
  )
  top <-  0
  # --------------------make a title grob; add it to graphic table---------------------------------
  if(!is.null(title)){
    top <- 1
    title_grob <- grid::textGrob(label = title, gp = grid::gpar(col = "black", fontsize = title_font_sz, fontface = 2L))
    graphic_table <- gtable::gtable_add_grob(
      x = graphic_table,
      grobs = title_grob,
      t = top,
      l = 1,
      r = length(col_widths)
    )
  }
  # -------------------make a single row of headings from df_char data-----------------
  headings <- colnames(df_char)
  heading_grobs <- lapply(headings, make_cell_grob, frgrd_color = head_color, bkgrd_color = head_fill, font_size = head_font_sz, font_face = 2L)
  top <- top + 1
  for(i in seq(1,length(heading_grobs), by = 1)){
    graphic_table <- gtable::gtable_add_grob(
      x = graphic_table,
      grobs = heading_grobs[[i]],
      t = top,
      l = i
    )
  }

  # ----------------------add a segment line below the headings-----------------------------------
  segment_grob <- grid::segmentsGrob(
    x0 = unit(0, units = "npc"),
    y0 = unit(0, units = "npc"),
    x1 = unit(1, units = "npc"),
    y1 = unit(0, units = "npc"),
    gp = gpar(lwd = 6, fill = NA)
  )
  top <- top + 1
  graphic_table <- gtable::gtable_add_grob(
    x = graphic_table,
    grobs = segment_grob,
    t = top,
    l = 1,
    r = ncol(df_char)
  )

  # ---------------------------add the cell values-----------------------------------
  cells_v <-  as.vector(unlist(t(df_char)))
  cells_grobs <- lapply(cells_v, make_cell_grob,
                        bkgrd_color = cell_fill,
                        frgrd_color = cell_color,
                        font_size = cell_font_sz,
                        just = cell_just,
                        x_val = cell_hor_pos)
  for(i in seq(1, nrow(df_char), by = 1)){
    for(ii in seq(1, ncol(df_char), by = 1)){
      graphic_table <- gtable::gtable_add_grob(
        x = graphic_table,
        grobs = cells_grobs[[(i - 1) * ncol(df_char) + ii]],
        t = top + i,
        l = ii
      )
    }
  }

  a_plot <- ggplotify::as.ggplot(graphic_table)
  return(a_plot)
}
# define a function for creating a cell grob
make_cell_grob <- function(
  cell_str = "",
  bkgrd_color = "white",
  frgrd_color = "black",
  font_size = 11,
  font_face = 1,
  just = "center",
  x_val = 0.5){
    cell_grob <- grid::grobTree(
      grid::rectGrob(gp = gpar(fill = bkgrd_color, alpha = 0.5, lwd = 1.5)),
      grid::textGrob(label = cell_str, gp = gpar(col = frgrd_color, fontsize = font_size, fontface = font_face), just = just,  x = unit(x_val, "npc"))
    )
    return(cell_grob)
}
