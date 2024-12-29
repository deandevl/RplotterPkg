
test_that("The required dataframe/vector parameter is submitted", {
  proportions_v <- c(
    var1=10, var2=40, var3=20, var4=50, var5=5,
    var6=30, var7=10, var8=67, var9=42, var10=33,
    var11=7, var12=35, var13=22, var14=90, var15=43
  )

  RplotterPkg::create_waffle_chart(
    x = proportions_v,
    title = "Test For 15 Proportion Variables",
    cell_color = "white",
    cell_fill = "brown",
    cell_nonfill = "lightgray",
    cell_alpha = 0.5,
    line_width = 4
  )
})
