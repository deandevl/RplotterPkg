# create_table()

    Code
      a_table <- RplotterPkg::create_table(df = RplotterPkg::people,
      container_width_px = 500, col_label_lst = list(col_0 = "ID", col_1 = "First Name",
        col_2 = "Last Name", col_3 = "Age", col_4 = "Address", col_5 = "Spouse"),
      decimals_lst = list(cols = 4, decimal = 0), rowname_col = "col_0", source_note = "Source: *Chardon City Directory*",
      footnote_title = "Employees are current", footnote_col_head_lst = list(
        footnote = "As reported on their job application", col = c(2, 3)),
      footnote_body_lst = list(footnote = "Age is approximated", col = 4, row = c(1,
        2)))

