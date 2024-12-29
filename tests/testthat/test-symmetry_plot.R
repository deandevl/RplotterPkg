
test_that("Reading data and defining 'df' parameter which is required", {

  # Show degree of symmetry in farm counts across US states
  show(RplotterPkg::symmetry_plot(
    df = RplotterPkg::farms,
    title = "Symmetry in farm counts across US states",
    var_name = "count",
    rot_y_tic_label = TRUE,
    line_linetype = "dotted"
  ))

})
