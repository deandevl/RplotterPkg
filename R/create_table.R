#' @title create_table
#'
#' @description Function wraps the gt package for creating tables from a data.frame object.
#'   Function incorporates the \href{https://cran.r-project.org/web/packages/gt/index.html}{gt}
#'   package to create tables primarily for R Markdown/Quarto documents.  The function offers a
#'   quick alternative if just limited styling and formatting are required.
#'
#' @param df A data.frame from which to display a table.
#' @param title A string that sets the table's title. The string can contain R markdown/html style syntax.
#' @param subtitle A string that sets the table's subtitle. The string can contain R markdown/html style syntax.
#' @param container_width_px A numeric that sets the overall container width in pixels.
#' @param container_height_px A numeric that sets the overall container height in pixels.
#' @param rowname_col The column name from \code{x} to use as row captions to be placed
#'   in the display table stub.
#' @param col_label_lst A list that resets the table's column labels. The list's key is the old label
#'   and the value is the new label. Initially the old labels will be \code{x}'s column names.
#' @param col_width_lst A list that sets the table's column widths. The list's key is a column's index
#'   number and value is the column width in pixels. Both the key and value are strings.
#' @param header_line A logical which if TRUE will place a bold, thick line between the header row and table body.
#' @param caption A string that sets the table caption which appears above the title
#'   to use as cross-referencing in R Markdown/Quarto document. The string can contain R markdown/html style syntax.
#' @param source_note A string located at the bottom of the table that notes the table's source.
#'   The string can contain R markdown/html style syntax.
#' @param hor_scroll_bar A logical that controls the appearance of the horizontal scroll bar.
#' @param ver_scroll_bar A logical that controls the appearance of the vertical scroll bar.
#' @param decimals_lst A list that describes the decimals for one or more table columns. The list should
#'   have the following key-value pairs:
#' \enumerate{
#'   \item cols = (an integer vector of column indices from \code{x})
#'   \item decimal = (an integer that defines the number of decimals for the column(s))
#' }
#' @param footnote_title A string at the bottom of the table that footnotes the table's title.
#' @param footnote_col_head_lst A list that describes the text/location of a footnote for column headings.
#'   The list should have the following key-value pairs:
#' \enumerate{
#'   \item footnote = (the footnote text to be displayed at the bottom of the table)
#'   \item col = (an integer vector setting the footnote's column heading index location's)
#' }
#' @param footnote_body_lst A list that describes the text/location of a footnote within the body of the table.
#'   The list should have the following key-value pairs:
#' \enumerate{
#'   \item footnote = (the footnote text to be displayed at the bottom of the table)
#'   \item col = (an integer vector setting the footnote's column index location's)
#'   \item row = (an integer vector setting the footnote's row index location's)
#' }
#'
#' @return A \code{gt} object of class "gt_tbl" which can be further modified.
#'
#' @examples
#' library(gt)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_table(
#'   df = RplotterPkg::people,
#'   container_width_px = 500,
#'   col_label_lst = list(
#'     col_0 = "ID",
#'     col_1 = "First Name",
#'     col_2 = "Last Name",
#'     col_3 = "Age",
#'     col_4 = "Address",
#'     col_5 = "Spouse"
#'   ),
#'   decimals_lst = list(
#'     cols = 4,
#'     decimal = 0
#'   ),
#'   rowname_col = "ID",
#'   source_note = "Source: *Chardon City Directory*",
#'   footnote_title = "Employees are current",
#'   footnote_col_head_lst = list(
#'     footnote = "As reported on their job application",
#'     col = c(2,3)
#'   ),
#'   footnote_body_lst = list(
#'     footnote = "Age is approximated",
#'     col = 4,
#'     row = c(1,2)
#'   )
#' )
#'
#' @import gt
#' @importFrom glue glue
#' @importFrom purrr map2
#'
#' @export
create_table <- function(
  df = NULL,
  title = NULL,
  subtitle = NULL,
  container_width_px = NULL,
  container_height_px = NULL,
  rowname_col = NULL,
  col_label_lst = NULL,
  col_width_lst = NULL,
  header_line = FALSE,
  caption = NULL,
  source_note = NULL,
  hor_scroll_bar = FALSE,
  ver_scroll_bar = FALSE,
  decimals_lst = NULL,
  footnote_title = NULL,
  footnote_col_head_lst = NULL,
  footnote_body_lst = NULL
){
  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
  }

  # table title
  if(!is.null(title)){
    title = md(title)
  }

  # table subtitle
  if(!is.null(subtitle)){
    subtitle = md(subtitle)
  }

  # table caption
  if(!is.null(caption)){
    caption = md(caption)
  }

  # initialize gt with title,subtitle,caption
  table_gt <- gt(
    data = df,
    rowname_col = rowname_col,
    caption = caption
  ) |>
  tab_header(
    title = title,
    subtitle = subtitle
  )

  # overall table options-- scroll bars,container width/height,table width
  ## scroll bars
  table_gt <- table_gt |>
    tab_options(
      container.overflow.x = hor_scroll_bar,
      container.overflow.y = ver_scroll_bar,
    )
  ## container width,height
  if(!is.null(container_width_px)){
    table_gt <- table_gt |>
      tab_options(container.width = gt::px(container_width_px))
  }
  if(!is.null(container_height_px)){
    table_gt <- table_gt |>
      tab_options(container.width = gt::px(container_height_px))
  }

  # column widths
  make_col_formula <- function(col, px){
    formula_str <- paste0(col, "~", "px(", px, ")")
    stats::as.formula(glue::glue(formula_str))
  }
  if(!is.null(col_width_lst)){
    cols <- names(col_width_lst)
    pix <- unlist(col_width_lst)

    formula_lst <- purrr::map2(cols, pix, make_col_formula)

    table_gt <- table_gt |>
      cols_width(.list = unlist(formula_lst))
  }

  #header labeling
  ## relabel column headings
  if(!is.null(col_label_lst)){
    table_gt <-  table_gt |>
      cols_label(.list = col_label_lst)
  }
  ## show thick line below header row?
  if(header_line){
    header_style_lst <- list(
      cell_borders(sides = "bottom", weight = px(4)),
      cell_text(weight = "bold")
    )
  }

  # column decimals
  if(!is.null(decimals_lst)){
    table_gt <-  table_gt |>
      fmt_number(
        columns = decimals_lst$cols,
        decimals = decimals_lst$decimal
      )
  }

  # source note
  if(!is.null(source_note)){
    table_gt <-  table_gt |>
      tab_source_note(
        source_note = md(source_note)
      )
  }
  ## source note formatting
  table_gt <- table_gt |>
    tab_options(
      source_notes.font.size = "xx-small"
    )

  # footnotes
  ## title footnote
  if(!is.null(title) & !is.null(footnote_title)){
    table_gt <- table_gt |>
      tab_footnote(
        footnote = footnote_title,
        locations = cells_title(groups = "title")
      )
  }
  ## column head footnotes
  if(!is.null(footnote_col_head_lst)){
    table_gt <- table_gt |>
      tab_footnote(
        footnote = footnote_col_head_lst$footnote,
        locations = cells_column_labels(columns = footnote_col_head_lst$col)
      )
  }
  ## table body footnotes
  if(!is.null(footnote_body_lst)){
    table_gt <- table_gt |>
      tab_footnote (
        footnote = footnote_body_lst$footnote,
        locations = cells_body(columns = footnote_body_lst$col, rows = footnote_body_lst$row)
      )
  }
  ## formatting footnotes
  table_gt <- table_gt |>
    tab_options(
      footnotes.font.size = "xx-small"
    )

  # tab_style
  ## style for body cells--fontsize
  table_gt <- table_gt |>
    tab_style(
      style = cell_text(size = "xx-small"),
      locations = cells_body(columns = everything(), rows = everything())
    )
  ## style for header labels--weight,fontsize
  header_style_lst <- list(
    cell_text(weight = "bold", size = "small")
  )
  table_gt <- table_gt |>
    tab_style(
      style = header_style_lst,
      locations = cells_column_labels(columns = everything())
    )

  return(table_gt)
}
