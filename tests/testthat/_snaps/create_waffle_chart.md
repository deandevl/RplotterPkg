# create_waffle_chart() named vector

    Code
      proportions_v <- c(var1 = 10, var2 = 40, var3 = 20, var4 = 50, var5 = 5, var6 = 30,
        var7 = 10, var8 = 67, var9 = 42, var10 = 33, var11 = 7, var12 = 35, var13 = 22,
        var14 = 90)
      a_plot <- RplotterPkg::create_waffle_chart(x = proportions_v, title = "Test For 14 Proportion Variables(vector)")

# create_waffle_chart() dataframe

    Code
      df <- data.frame(variables = c("var1", "var2", "var3", "var4", "var5", "var6",
        "var7"), proportions = c(40, 8, 23, 55, 71, 0, 22))
      a_plot <- RplotterPkg::create_waffle_chart(x = df, name_col = "variables",
        prop_col = "proportions", title = "Test For 7 Proportion Variables(dataframe)")

