
test_that("The required dataframe/aes_x/aes_y parameters are submitted and
  a .png file of the plot is created.", {

  plot_file <- "range_plot.png"

  a_plot <- RplotterPkg::create_range_plot(
    df = RplotterPkg::penguins_stats,
    aes_x = "species",
    aes_y = "avg_body_mass",
    aes_y_min = "min_body_mass",
    aes_y_max = "max_body_mass",
    title = "Average and Range of Penguins Body Mass(g) by Species",
    subtitle = "Source: palmerpenguins",
    center_titles = TRUE,
    x_title = "Species",
    y_title = "Body Mass(g)",
    rot_y_tic_label = TRUE,
    pts_fill = "blue",
    pts_shape = 22,
    pts_stroke = 1.7,
    line_width = 1.5,
    fatten_pts = 6,
    line_type = "solid",
    line_pts_color = "red",
    line_pts_alpha = 0.5,
    y_limits = c(2500, 7000),
    y_major_breaks = seq(2500,7000,500),
    show_major_grids = TRUE,
    show_minor_grids = FALSE,
    do_coord_flip = TRUE,
    png_file_path = plot_file
  )
  show(a_plot)

  expect_true(is.ggplot(a_plot))
  expect_true(file.exists(plot_file))
  file.remove(plot_file)
})
