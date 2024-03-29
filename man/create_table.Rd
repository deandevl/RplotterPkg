% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_table.R
\name{create_table}
\alias{create_table}
\title{Function wraps the gt package for creating tables from a data.frame object}
\usage{
create_table(
  x,
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
  footnote_body_lst = NULL,
  display_table = TRUE
)
}
\arguments{
\item{x}{A data.frame from which to display a table.}

\item{title}{A string that sets the table's title. The string can contain R markdown/html style syntax.}

\item{subtitle}{A string that sets the table's subtitle. The string can contain R markdown/html style syntax.}

\item{container_width_px}{A numeric that sets the overall container width in pixels.}

\item{container_height_px}{A numeric that sets the overall container height in pixels.}

\item{rowname_col}{The column name from \code{x} to use as row captions to be placed
in the display table stub.}

\item{col_label_lst}{A list that resets the table's column labels. The list's key is the old label
and the value is the new label. Initially the old labels will be \code{x}'s column names.}

\item{col_width_lst}{A list that sets the table's column widths. The list's key is a column's index
number and value is the column width in pixels. Both the key and value are strings.}

\item{header_line}{A logical which if TRUE will place a bold, thick line between the header row and table body.}

\item{caption}{A string that sets the table caption which appears above the title
to use as cross-referencing in R Markdown/Quarto document. The string can contain R markdown/html style syntax.}

\item{source_note}{A string located at the bottom of the table that notes the table's source.
The string can contain R markdown/html style syntax.}

\item{hor_scroll_bar}{A logical that controls the appearance of the horizontal scroll bar.}

\item{ver_scroll_bar}{A logical that controls the appearance of the veritical scroll bar.}

\item{decimals_lst}{A list that describes the decimals for one or more table columns. The list should
have the following key-value pairs:
\enumerate{
\item cols = (an integer vector of column indices from \code{x})
\item decimal = (an integer that defines the number of decimals for the column(s))
}}

\item{footnote_title}{A string at the bottom of the table that footnotes the table's title.}

\item{footnote_col_head_lst}{A list that describes the text/location of a footnote for column headings.
The list should have the following key-value pairs:
\enumerate{
\item footnote = (the footnote text to be displayed at the bottom of the table)
\item col = (an integer vector setting the footnote's column heading index location's)
}}

\item{footnote_body_lst}{A list that describes the text/location of a footnote within the body of the table.
The list should have the following key-value pairs:
\enumerate{
\item footnote = (the footnote text to be displayed at the bottom of the table)
\item col = (an integer vector setting the footnote's column index location's)
\item row = (an integer vector setting the footnote's row index location's)
}}

\item{display_table}{A logical which if TRUE (the default) will display the table.
Otherwise a \code{gt} object of class "gt_tbl" is returned which can be further modified.}
}
\value{
A \code{gt} object of class "gt_tbl" which can be further modified.
}
\description{
Function incorporates the \href{https://cran.r-project.org/web/packages/gt/index.html}{gt}
package to create tables primarily for R Markdown/Quarto documents.  The function offers a
quick alternative if just limited styling and formatting are required.
}
\author{
Rick Dean
}
