#' Function creates a table for tabular data in rmarkdown documents
#'
#' @description The function is a wrapper around kableExtra::kbl() for
#'  defaulting many of the options available in creating tables in rmarkdown.
#'  A \code{knitr_kable} is returned where further modification can be applied.
#'
#' @param x A data frame or matrix with tabular data.
#' @param format A string that sets the tables format. Acceptable values
#'  are \dQuote{html} or \dQuote{pdf}.
#' @param col_names A string vector of column names for the table. If NULL
#'  then names are taken from \code{x}.
#' @param font_size A numeric that sets the table's font size.
#' @param caption A string that sets the table's caption.
#' @param caption_sz A numeric that sets the font size of the caption in pixels.
#' @param align_v A character vector that sets the alignment for the data under the headings.
#'  Acceptable values are \dQuote{c}, \dQuote{l}, \dQuote{r} for each of the columns.
#' @param position A string that sets the placement of the table. Acceptable
#'  values are \dQuote{center}, \dQuote{left}, \dQuote{right}.
#' @param full_width A logical which if TRUE, the table takes the full width of
#'  the documents available width.
#' @param fixed_thead A logical which if TRUE fixes the header as scrolling takes place.
#' @param head_bkgd A string that sets the header's background color.
#' @param head_col A string that sets the header's color.
#' @param head_sz A numeric that sets the header's font size.
#' @param head_angle A numeric that sets the header's angle.
#' @param footnote A string which if not NULL adds a general footnote to the table.
#' @param footnote_title A string that sets the footnote title.
#' @param borders A logical which if TRUE will place borders along the rows and columns.
#' @param border_sz A numeric that defines the border width in pixels.
#' @param border_col A string that sets the border color.
#' @param scroll_height A string that sets the scroll height in pixels. For example:
#'  \dQuote{200px}
#' @param scroll_width A string that sets the scroll width in pixels. For example:
#'  \dQuote{200px}
#' @param hide_ver_scrollbar A logical which if TRUE will hide the vertical scroll bar.
#' @param hide_hor_scrollbar A logical which if TRUE will hide the horizontal scroll bar.
#'
#' @importFrom kableExtra kable_paper
#' @importFrom kableExtra kbl
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra footnote
#' @importFrom kableExtra scroll_box
#'
#' @return A "kableExtra"-"knitr_kable" object
#'
#' @author Rick Dean
#'
#' @export
create_table <- function(
  x,
  format = "html",
  col_names = NULL,
  font_size = NULL,
  caption = NULL,
  caption_sz = 20,
  align_v = NULL,
  position = "center",
  full_width = F,
  fixed_thead = F,
  head_bkgd = "white",
  head_col = "black",
  head_sz = 18,
  head_angle = 0,
  footnote = NULL,
  footnote_title = NULL,
  borders = FALSE,
  border_sz = 2,
  border_col = "black",
  scroll_height = NULL,
  scroll_width = NULL,
  hide_ver_scrollbar = FALSE,
  hide_hor_scrollbar = FALSE
){

  if(!is.null(caption)){
    caption <- paste0(
      "<center>",
      '<div style="font-weight:bold;font-size:',
      caption_sz,'px; color:black">',
      caption,
      "</div></center>")
  }

  if(is.null(col_names)){
    col_names <- colnames(x)
  }

  a_table <- kableExtra::kbl(
    x = x,
    format = "html",
    caption = caption,
    col.names = col_names,
    align = align_v
  )

  a_table <- kableExtra::row_spec(
    kable_input = a_table,
    row = 0,
    angle = head_angle,
    background = head_bkgd,
    color = head_col,
    font_size = head_sz,
    bold = T,
    extra_css = paste0("border-bottom: 4px solid ",border_col,";")
  )

  if(borders){
    a_table <- kableExtra::row_spec(
      kable_input = a_table,
      row = 1:nrow(x),
      extra_css = paste0("border-bottom: ", border_sz,"px solid ",border_col,";")
    )

    a_table <- kableExtra::column_spec(
      kable_input = a_table,
      column = 1:(ncol(x)+1),
      border_left = paste0(border_sz, "px solid ", border_col),
      border_right = paste0(border_sz, "px solid ", border_col)
    )
  }

  if(!is.null(footnote) & !is.null(footnote_title)){
    a_table <- kableExtra::footnote(
      kable_input = a_table,
      general = footnote,
      general_title = footnote_title
    )
  }

  a_table <- kableExtra::kable_paper(
    kable_input = a_table,
    full_width = full_width,
    position = position,
    font_size = font_size
  )

  if(hide_ver_scrollbar | hide_hor_scrollbar | !is.null(scroll_height) | !is.null(scroll_width)){
    extra_css <- NULL
    if(hide_hor_scrollbar & hide_ver_scrollbar){
      extra_css <- "overflow-y: hidden; overflow-x: hidden;"
    }else if(hide_ver_scrollbar){
      extra_css <- "overflow-y: hidden;"
    }else if(hide_hor_scrollbar){
      extra_css <- "overflow-x: hidden;"
    }
    a_table <- kableExtra::scroll_box(
      height = scroll_height,
      width = scroll_width,
      kable_input = a_table,
      fixed_thead = fixed_thead,
      extra_css = extra_css
    )
  }

  return(a_table)
}
