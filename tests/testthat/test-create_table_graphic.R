
test_that("The required dataframe", {
  graph_table <- RplotterPkg::create_table_graphic(
    df = RplotterPkg::car_stats,
    table_width = 7,
    show_row_names = TRUE,
    cell_just = "right",
    cell_hor_pos = 0.95,
    title = "Average Measures by Cylinders",
    display_plot = FALSE
  )
  show(plot(graph_table))

  expect_true(!is.null(graph_table))
})
