
test_that("create_table_graphic()", {
  graph_table <- RplotterPkg::create_table_graphic(
    df = RplotterPkg::car_stats,
    table_width = 7,
    show_row_names = TRUE,
    cell_just = "right",
    cell_hor_pos = 0.95,
    title = "Average Measures by Cylinders"
  )

  expect_true(is.ggplot(graph_table))
  vdiffr::expect_doppelganger("create_table_graphic()", graph_table)
  expect_no_error(ggplot_build(graph_table))
})
