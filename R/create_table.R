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
#' @param align A string that sets the alignment for the data under the headings.
#'  Acceptable values are \dQuote{c}, \dQuote{l}, \dQuote{r}.
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
#' @param scroll_height A string that sets the scroll height in pixels. For example:
#'  \dQuote{200px}
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
  x = NULL,
  format = "html",
  col_names = NULL,
  font_size = NULL,
  caption = NULL,
  caption_sz = 20,
  align = "c",
  position = "center",
  full_width = F,
  fixed_thead = F,
  head_bkgd = "white",
  head_col = "black",
  head_sz = 18,
  head_angle = 0,
  footnote = NULL,
  footnote_title = NULL,
  scroll_height = NULL
){
  if(is.null(col_names)){
    col_names <- colnames(x)
  }
  if(!is.null(caption)){
    caption <- paste0(
      "<center>",
      '<div style="font-weight:bold;font-size:',
      caption_sz,'px; color:black">',
      caption,
      "</div></center>")
  }
  a_table <- kableExtra::kbl(
    x = x,
    format = format,
    caption = caption,
    col.names = col_names,
    align = rep(align, ncol(x))
  )
  a_table <- kableExtra::kable_paper(
    kable_input = a_table,
    full_width = full_width,
    position = position,
    font_size = font_size,
    fixed_thead = fixed_thead
  )
  a_table <- kableExtra::row_spec(
    kable_input = a_table,
    row = 0,
    angle = head_angle,
    background = head_bkgd,
    color = head_col,
    font_size = head_sz,
    bold = T
  )

  if(!is.null(footnote)){
    a_table <- kableExtra::footnote(
      kable_input = a_table,
      general = footnote,
      general_title = footnote_title
    )
  }
  if(!is.null(scroll_height)){
    a_table <- kableExtra::scroll_box(
      kable_input = a_table,
      height = scroll_height
    )
  }

  return(a_table)
}
