# The returned list object is not NULL

    Code
      mtcars_plot <- ggplot2::ggplot(data = datasets::mtcars, ) + ggplot2::geom_point(
        aes(x = mpg, y = wt, color = cyl), size = 3)
      legend_right <- RplotterPkg::get_grob_component(a_plot = mtcars_plot,
        component_name = "guide-box-right")

