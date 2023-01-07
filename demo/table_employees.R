library(gt)
library(data.table)
library(RplotterPkg)

# Create a data.frame:
dt <- data.table::data.table(
  col_0 = c(456, 243,765,321),
  col_1 = c("Rick", "John", "Sally", "Jim"),
  col_2 = c("Smith", "Taylor", "Jones", "Reed"),
  col_3 = c(34.5, 63.54, 37.78, 29.3),
  col_4 = c("183 Sunset Blvd. Los Angeles, California",
            "98 Spalding Avenue, Bardstown, Kentucky",
            "173 Columbia Street, Austin, Texas",
            "1003 Frankfort Road, Louisville, Kentucky"
  ),
  col_5 = c("Judy","Tom","Bill","Sharon")
)

# Show the table:
RplotterPkg::create_table(
  x = dt,
  container_width_px = 500,
  col_label_lst = list(
    col_0 = "ID",
    col_1 = "First Name",
    col_2 = "Last Name",
    col_3 = "Age",
    col_4 = "Address",
    col_5 = "Spouse"
  ),
  # col_width_lst = list(
  #   "1" = "75",
  #   "2" = "125",
  #   "3" = "75",
  #
  # ),
  decimals_lst = list(
    cols = 4,
    decimal = 0
  ),
  rowname_col = "ID",
  source_note = "Source: *Chardon City Directory*",
  footnote_title = "Employees are current",
  footnote_col_head_lst = list(
    footnote = "As reported on their job application",
    col = c(2,3)
  ),
  footnote_body_lst = list(
    footnote = "Age is approximated",
    col = 4,
    row = c(1,2)
  )
)

