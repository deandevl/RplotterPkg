
test_that("create_table() namespaces", {
  expect_true(requireNamespace("gt", quietly = TRUE))
  expect_true(requireNamespace("glue", quietly = TRUE))
  expect_true(requireNamespace("purrr", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("create_table()", {
  expect_snapshot({
    a_table <- RplotterPkg::create_table(
      df = RplotterPkg::people,
      container_width_px = 500,
      col_label_lst = list(
        col_0 = "ID",
        col_1 = "First Name",
        col_2 = "Last Name",
        col_3 = "Age",
        col_4 = "Address",
        col_5 = "Spouse"
      ),
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
  })
  expect_s3_class(a_table, "gt_tbl")
})
