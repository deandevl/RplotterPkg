
test_that("Parameter 'x' is a list and required", {

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
  table_grob <- RplotterPkg::stem_leaf_display(
    x = marathon_times_lst,
    title = "Women times(min) in Boston marathon",
    heading_color = "#FF5500",
    display_grob = FALSE
  )
  grid::grid.newpage()
  grid::grid.draw(table_grob)

  expect_true(!is.null(marathon_times_lst))
  expect_true(is.list(marathon_times_lst))
})
