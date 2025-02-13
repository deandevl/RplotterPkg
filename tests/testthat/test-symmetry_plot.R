
test_that("symmetry_plot() namespaces", {
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("symmetry_plot()", {
  expect_snapshot({
    # Show degree of symmetry in farm counts across US states
    a_plot <- RplotterPkg::symmetry_plot(
      df = RplotterPkg::farms,
      title = "Symmetry in farm counts across US states",
      var_name = "count",
      rot_y_tic_label = TRUE,
      line_linetype = "dotted"
    )
  })
  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("symmetry_plot()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
