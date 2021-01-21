#' Function create a box plot.
#'
#' @description Function creates a ggplot2 based box plot with optional scaling, ordering and outlier viewing.
#'
#' @param df The target data frame for the density chart.
#' @param aes_x A string that sets the x axis factor variable name from \code{df}.
#' @param aes_y A string that sets the y axis variable name from \code{df}.
#' @param aes_color A string that sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill A string that sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param box_fill A string that sets the fill color for the box plot.
#' @param box_color A string that sets the color for the box plot.
#' @param box_size A numeric that sets the size of the box outline.
#' @param box_alpha A numeric that set the alpha component to \code{box_color}.
#' @param palette_colors A character vector to set the palette colors.
#' @param x_limits A character vector that defines possible values of the scale and their order..
#' @param x_breaks A character vector of x axis breaks or a function that takes the limits as input and returns breaks as output.
#' @param x_labels A character vector that defines the x axis tic labels. Vector must be the same length as \code{x_breaks}.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param coord_flip A logical which if TRUE will flip the x and y axis'.
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  \dQuote{top}, \dQuote{bottom}, \dQuote{left}, \dQuote{right}.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param show_outliers A logical which if TRUE will display the outliers.
#' @param label_outlier_var A string that sets the variable name from \code{df} whose value will to be displayed corresponding
#' to the \code{aes_y} outlier. If \code{show_outliers} is TRUE and this parameter is NULL, then the row number will be used as the label.
#' @param label_outlier_color A string that sets the color of outlier labels.
#' @param order_by_median A string which will order the plot of \code{aes_x} factor levels/categories by the \code{aes_y} medians.
#'  Acceptable values are \dQuote{asc} or \dQuote{desc}
#' @param ol_color A string that sets the outlier color.
#' @param ol_fill A string that sets the outlier fill.
#' @param ol_size A numeric that set the outlier size.
#' @param ol_shape A string that set the outlier shape.
#' @param ol_stroke A numeric that sets the outlier shape line width.
#' @param ol_alpha A numeric that sets the outlier alpha for color.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#'  are removed.
#'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym
#' @importFrom ggrepel geom_text_repel
#' @import ggplot2

#'
#' @return A plot object.
#'
#' @author Rick Dean
#'
#' @export
create_box_plot <- function(
  df,
  aes_x,
  aes_y,
  aes_color = NULL,
  aes_fill = NULL,
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_title = aes_x,
  y_title = aes_y,
  rot_x_tic_angle = 0,
  rot_y_tic_label = FALSE,
  box_fill = NA,
  box_color = "black",
  box_size = 1.0,
  box_alpha = 1.0,
  palette_colors = NULL,
  x_limits = NULL,
  x_breaks = waiver(),
  x_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  coord_flip = FALSE,
  do_x_title = TRUE,
  do_y_title = TRUE,
  show_legend = TRUE,
  legend_pos = "top",
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  show_outliers = FALSE,
  label_outlier_var = NULL,
  label_outlier_color = "black",
  ol_color = "black",
  ol_fill = "black",
  ol_size = 1.5,
  ol_shape = 19,
  ol_stroke = 0.5,
  ol_alpha = NULL,
  order_by_median = NULL,
  silent_NA_warning = FALSE) {

  df_copy <- data.table::as.data.table(df, keep.rownames = T)

  if(!is.null(order_by_median)){
    x_group_medians_df <- df_copy[,.(x_medians = stats::median(get(aes_y), na.rm = T)), by = aes_x]

    if(order_by_median == "asc") {
      x_group_medians_df <- data.table::setorder(x_group_medians_df, x_medians)
    }else {
      x_group_medians_df <- data.table::setorder(x_group_medians_df, -x_medians)
    }
    col_x <- factor(df_copy[[aes_x]], levels = x_group_medians_df[[aes_x]])
    df_copy[, c(aes_x) := col_x]
  }

  aplot <- ggplot2::ggplot(data = df_copy, aes(x = !!rlang::sym(aes_x), y = !!rlang::sym(aes_y)))

  if(!is.null(label_outlier_var)){
    show_outliers <- TRUE
  }
  if(!show_outliers) {
    ol_shape <- NA
  }

  if(!is.null(aes_color)) {
    aplot <- aplot + geom_boxplot(aes(color = !!rlang::sym(aes_color)), fill = box_fill, alpha = box_alpha, size = box_size,
                                outlier.color = ol_color,
                                outlier.fill = ol_fill,
                                outlier.size = ol_size,
                                outlier.shape = ol_shape,
                                outlier.stroke = ol_stroke,
                                outlier.alpha = ol_alpha,
                                na.rm = silent_NA_warning)
  }else if(!is.null(aes_fill)){
    aplot <- aplot + geom_boxplot(aes(fill = !!rlang::sym(aes_fill)), color = box_color, alpha = box_alpha, size = box_size,
                                outlier.color = ol_color,
                                outlier.fill = ol_fill,
                                outlier.size = ol_size,
                                outlier.shape = ol_shape,
                                outlier.stroke = ol_stroke,
                                outlier.alpha = ol_alpha,
                                na.rm = silent_NA_warning)
  }else {
    aplot <- aplot + geom_boxplot(fill = box_fill, color = box_color, alpha = box_alpha, size = box_size,
                                outlier.color = ol_color,
                                outlier.fill = ol_fill,
                                outlier.size = ol_size,
                                outlier.shape = ol_shape,
                                outlier.stroke = ol_stroke,
                                outlier.alpha = ol_alpha,
                                na.rm = silent_NA_warning)
  }

  if(!is.null(palette_colors)){
    if(!is.null(aes_fill)) {
      aplot <- aplot + scale_fill_manual(values = palette_colors)
    }else if(!is.null(aes_color)) {
      aplot <- aplot + scale_color_manual(values = palette_colors)
    }
  }

  if(coord_flip){
    aplot <- aplot + coord_flip()
  }

  if(!show_legend){
    aplot <- aplot +
      theme(
        legend.position = "none"
      )
  }else {
    aplot <- aplot +
      theme(
        legend.position = legend_pos
      )
  }

  aplot <- aplot +
    theme(
      panel.background = element_rect(fill = "white", color = "black")
    )

  if(show_major_grids){
    aplot <- aplot +
      theme(
        panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray")
      )
  }
  if(show_minor_grids){
    aplot <- aplot +
      theme(
        panel.grid.minor = element_line(size = 0.5, linetype = "solid", color = "gray")
      )
  }

  if(center_titles) {
    aplot <- aplot +
      theme(
        plot.title = element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = .5, size = 14)
      )
  }else {
    aplot <- aplot +
      theme(
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14)
      )
  }

  aplot <- aplot + labs(title = title, subtitle = subtitle)
  if(do_x_title){
    aplot <- aplot +
      labs(x = x_title)
  }else {
    aplot <- aplot +
      theme(
        axis.title.x = element_blank()
      )
  }
  if(do_y_title){
    aplot <- aplot +
      labs(y = y_title)
  }else {
    aplot <- aplot +
      theme(
        axis.title.y = element_blank()
      )
  }

  aplot <- aplot + scale_x_discrete(
      limits = x_limits,
      breaks = x_breaks,
      labels = x_labels
  )

  aplot <- aplot + scale_y_continuous(
    limits = y_limits,
    breaks = y_major_breaks,
    minor_breaks = y_minor_breaks,
    labels = y_labels
  )

  if(rot_y_tic_label){
    rot_y_tic_angle <- 0
  }else{
    rot_y_tic_angle <- 90
  }
  aplot <- aplot +
    theme(
      axis.text.x = element_text(size = axis_text_size, color = "black"),
      axis.title.x = element_text(size = axis_text_size + 2, color = "black"),
      axis.text.y = element_text(size = axis_text_size, color = "black", angle = rot_y_tic_angle),
      axis.title.y = element_text(size = axis_text_size + 2, color = "black")
    )
  if(rot_x_tic_angle > 0){
    aplot <- aplot +
      theme(axis.text.x = element_text(angle = rot_x_tic_angle, hjust = 1.0))
  }

  if(show_outliers){
    df_copy[, `:=`(lower_outlier = stats::quantile(get(aes_y), 0.25, na.rm = T) - 1.5 * stats::IQR(get(aes_y), na.rm = T),
                   upper_outlier = stats::quantile(get(aes_y), 0.75, na.rm = T) + 1.5 * stats::IQR(get(aes_y), na.rm = T)
    ), by = aes_x]

    if(!is.null(label_outlier_var)){
      df_copy[,`:=`(outlier = fifelse(get(aes_y) < lower_outlier | get(aes_y) > upper_outlier, as.character(get(label_outlier_var)), ""))]
    }else {
      df_copy[,`:=`(outlier = fifelse(get(aes_y) < lower_outlier | get(aes_y) > upper_outlier, as.character(rn), ""))]
    }
    aplot <- aplot +
      ggrepel::geom_text_repel(data = df_copy, aes(label = outlier), color = label_outlier_color, na.rm = TRUE)
  }
  return(aplot)
}
