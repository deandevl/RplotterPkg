
test_that("stem_leaf_display()", {

# ----------------------Women in Boston marathon---------------------
  # stem and leaf for marathon times of women across ages
  marathon_times_lst <- list(
    "age_20" = RplotterPkg::boston_marathon[age == 20,]$time,
    "age_30" = RplotterPkg::boston_marathon[age == 30,]$time,
    "age_40" = RplotterPkg::boston_marathon[age == 40,]$time,
    "age_50" = RplotterPkg::boston_marathon[age == 50,]$time,
    "age_60" = RplotterPkg::boston_marathon[age == 60,]$time
  )
  # display stem and leaf of women times across ages
  a_plot <- RplotterPkg::stem_leaf_display(
    x = marathon_times_lst,
    title = "Women times(min) in Boston marathon",
    heading_color = "#FF5500"
  )

  expect_true(is.ggplot(a_plot))
  vdiffr::expect_doppelganger("stem_leaf_display()", a_plot)
  expect_no_error(ggplot_build(a_plot))
})
