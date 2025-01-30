test_that("get_grob_component() namespaces", {
  expect_true(requireNamespace("ggplotify", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("vdiffr", quietly = TRUE))
})

test_that("The returned list object is not NULL", {
  expect_snapshot({
    mtcars_plot <- ggplot2::ggplot(
      data = datasets::mtcars,
    ) +
      ggplot2::geom_point(aes(x = mpg, y = wt, color = cyl), size = 3)

    legend_right <- RplotterPkg::get_grob_component(
      a_plot = mtcars_plot,
      component_name = "guide-box-right"
    )
  })
  expect_true(!is.null(legend_right))
})
