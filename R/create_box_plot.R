#' @title create_box_plot
#'
#' @description Function wraps around ggplot2 geom_boxplot to create a box plot.
#'
#' @param df The target data frame for the density chart.
#' @param aes_x An optional factor variable name from \code{df} that sets the x axis variable.
#'   Multiple box plots will be displayed along the x axis if this argument is set.
#' @param aes_y A required variable name from \code{df} that sets the y axis variable.
#' @param aes_color Sets the variable name from \code{df} for the aesthetic mapping for color.
#' @param aes_fill Sets the variable name from \code{df} for the aesthetic mapping for fill.
#' @param aes_label Sets the variable name from \code{df} whose value will to be displayed corresponding
#'  to the \code{aes_y} outliers.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title. If NULL (the default)  then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If NULL (the default)  then the y axis title does not appear.
#' @param hide_x_tics A logical that controls the appearance of the x axis tics.
#' @param hide_y_tics A logical that controls the appearance of the y axis tics.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic labels. When x tic labels are long,
#'  a value of 40 for this argument usually works well.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param box_fill A string that sets the fill color attribute for the box plot.
#' @param box_color A string that sets the color attribute for the box plot.
#' @param box_line_width A numeric that sets the size attribute of the box line width.
#' @param box_alpha A numeric that set the alpha component attribute to \code{box_color}.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_decimals A numeric that sets the number of decimal places for y-tic labels.
#' @param y_scientific A logical which if TRUE will put the y-tic labels in scientific notation.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param do_coord_flip A logical which if TRUE will flip the x and y axis'.
#' @param panel_color A string in hexidecimal or color name that sets the plot panel's color.
#'   The default is "white".
#' @param panel_border_color A string in hexidecimal or color name that sets the plot panel's border color.
#'   The default is "black".
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are
#'  "top", "bottom", "left", "right".
#' @param legend_key_width A numeric that sets the legend width in cm.
#' @param legend_key_height A numeric that sets the legend height in cm.
#' @param legend_key_backgrd A string that sets the legend's background color. The default is "white".
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param order_by_median A string which will order the plot of \code{aes_x} factor levels/categories by the \code{aes_y} medians.
#'  Acceptable values are \code{asc} or \code{desc}. Note that the optional argument \code{aes_x} must not be NULL.
#' @param ol_color A string that sets the outlier color.
#' @param ol_fill A string that sets the outlier fill.
#' @param ol_size A numeric that set the outlier size.
#' @param ol_shape A string that set the outlier shape.
#' @param ol_stroke A numeric that sets the outlier shape line width.
#' @param ol_alpha A numeric that sets the outlier alpha for color.
#' @param silent_NA_warning A logical that controls the appearance of a console warning when Na's
#'  are removed.
#' @param png_file_path A character string with the directory and file name to produce
#'  a png image of the plot.
#' @param png_width_height A numeric vector that sets the width and height of the png image in pixels. The
#'  default is c(480,480).  There are 37.8 pixels in a centimeter.
#'
#' @return A ggplot class object.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' library(RplotterPkg)
#'
#' RplotterPkg::create_box_plot(
#'   df = RplotterPkg::organdata,
#'   aes_x = "country",
#'   aes_y = "donors",
#'   aes_label = "donors",
#'   order_by_median = "desc",
#'   y_limits = c(5,35),
#'   y_major_breaks = seq(5, 35, 5),
#'   title = "Organ Donation Rate per Million",
#'   subtitle = "Showing outlier rates",
#'   x_title = "Country",
#'   y_title = "Donor Rate",
#'   do_coord_flip = TRUE,
#'   box_color = "purple",
#'   box_line_width = 0.8,
#'   rot_y_tic_label = TRUE,
#'   ol_color = "red",
#'   ol_size = 1.5
#' )
#'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym
#' @import ggplot2
#'
#' @export
create_box_plot <- function(
    df = NULL,
    aes_x = NULL,
    aes_y = NULL,
    aes_color = NULL,
    aes_fill = NULL,
    aes_label = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    center_titles = FALSE,
    x_title = NULL,
    y_title = NULL,
    hide_x_tics = FALSE,
    hide_y_tics = FALSE,
    rot_x_tic_angle = 0,
    rot_y_tic_label = FALSE,
    box_fill = NA,
    box_color = "black",
    box_line_width = 0.5,
    box_alpha = 1.0,
    y_limits = NULL,
    y_major_breaks = waiver(),
    y_minor_breaks = waiver(),
    y_decimals = 0,
    y_scientific = FALSE,
    axis_text_size = 11,
    do_coord_flip = FALSE,
    show_legend = TRUE,
    panel_color = "white",
    panel_border_color = "black",
    legend_pos = "top",
    legend_key_width = 0.7,
    legend_key_height = 0.7,
    legend_key_backgrd = "white",
    show_major_grids = TRUE,
    show_minor_grids = TRUE,
    ol_color = "black",
    ol_fill = "black",
    ol_size = 1.5,
    ol_shape = 19,
    ol_stroke = 0.5,
    ol_alpha = NULL,
    order_by_median = NULL,
    silent_NA_warning = FALSE,
    png_file_path = NULL,
    png_width_height = c(480,480)
  ){

  x <- outliers <- . <- x_medians <- NULL

  if(is.null(df)){
    stop("The dataframe(df) is a required parameter")
  }
  if(is.null(aes_y)){
    stop("aes_y is a required argument.")
  }

  if(!is.null(aes_fill)){
    aes_fill <- rlang::sym(aes_fill)
  }
  if(!is.null(aes_color)){
    aes_color <- rlang::sym(aes_color)
  }

  # -------------Set geom attribute default values------------------
  ggplot2::update_geom_defaults(
    "boxplot",
    list(
      fill = box_fill,
      color = box_color,
      linewidth = box_line_width,
      alpha = box_alpha
    )
  )

  # Convert df to a data.table
  dt <- data.table::as.data.table(df)

  # If aes_x is undefined, then add an "x" column for doing a single boxplot
  aes_x_is_undefined <- FALSE
  if(is.null(aes_x)){
    aes_x_is_undefined <- TRUE
    aes_x <- "x"
    dt[, x := 0]
  }

  # ---If we are labeling outliers, add a couple of new columns to dt
  if(!is.null(aes_label)){
    # ---Define a function for checking for outliers
    # Takes values of aes_y and returns TRUE/FALSE boolean vector if a certain point is an outlier or not
    # Function also takes in a coefficient
    check_outlier <- function(v, coef = 1.5){
      quantiles <- stats::quantile(v, probs = c(0.25, 0.75), na.rm = TRUE)
      IQR <- quantiles[2] - quantiles[1]
      lower_limit <- quantiles[1] - coef * IQR
      upper_limit <- quantiles[2] + coef * IQR
      result <- v < lower_limit | v > upper_limit
      return(result)
    }
    # ---Apply the function to dt (df converted to data.table) to create an "outliers" column
    dt[, outliers := check_outlier(get(aes_y)), by = aes_x]
    # ---Create a labels column for dt based on "outliers" column
    dt[, labels := ifelse(outliers, dt[[aes_label]], NA)]
    aes_label <- "labels"
  }

  # ------------Reorder dt aes_x factor levels?------------
  if(!is.null(order_by_median)){
    # Get the medians of aes_y for each aes_x group
    x_medians_dt <- dt[, .(x_medians = stats::median(as.numeric(get(aes_y)))), by = get(aes_x)]
    data.table::setnames(x_medians_dt, old="get", new = aes_x)

    # Sort the "x_medians" column
    if(order_by_median == "desc") {
      x_medians_dt <- data.table::setorder(x_medians_dt, -x_medians)
    }else {
      x_medians_dt <- data.table::setorder(x_medians_dt, x_medians)
    }

    # Resort the levels of dt's aes_x by levels of aes_x in x_medians_dt
    dt[[aes_x]] <- factor(dt[[aes_x]], levels = x_medians_dt[[aes_x]])
  }

  # ---Define the ggplot2 object with its data, aesthetics, with geom_boxplot and geom_text
  aplot <- ggplot(data = dt,
    aes(
      factor(!!rlang::sym(aes_x)),
      !!rlang::sym(aes_y)
    )
  ) +
  ggplot2::geom_boxplot(
    aes(
      color = !!aes_color,
      fill = !!aes_fill
    ),
    outlier.color = ol_color,
    outlier.fill = ol_fill,
    outlier.size = ol_size,
    outlier.shape = ol_shape,
    outlier.stroke = ol_stroke,
    outlier.alpha = ol_alpha,
    na.rm = silent_NA_warning
  )

  # -----------------Are we labeling the points?----------------
  if(!is.null(aes_label)){
    hjust <- 0.0
    vjust <- -0.3
    if(do_coord_flip){
      hjust <- -0.1
      vjust <- 0.0
    }
    aplot <- aplot +
      ggplot2::geom_text(
        aes(
          label = !!sym(aes_label),
          hjust = hjust,
          vjust = vjust
        ),
        na.rm = TRUE
      )
  }

  # -------------------Additional ggplot2 components------------------------
  # ----------------------title and subtitle-----------------
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
  aplot <- aplot + labs(title = title, subtitle = subtitle, caption = caption)

  # --------------------x/y axis titles----------------------------
  if(!is.null(aes_x)){
    if(is.null(x_title)) {
      aplot <- aplot +
      theme(
        axis.title.x = element_blank()
      )
    }else{
      aplot <- aplot +
        labs(x = x_title)
    }
  }
  if(is.null(y_title)) {
    aplot <- aplot +
      theme(
        axis.title.y = element_blank()
      )
  }else{
    aplot <- aplot +
      labs(y = y_title)
  }

  # --------------------panel and grids---------------------
  aplot <- aplot +
    theme(
      panel.background = element_rect(fill = panel_color, color = panel_border_color, linewidth = 2)
    )

  if(show_major_grids){
    aplot <- aplot +
      theme(
        panel.grid.major = element_line(linewidth = 0.2, linetype = "solid", color = "gray90")
      )
  }
  if(show_minor_grids){
    aplot <- aplot +
      theme(
        panel.grid.minor = element_line(linewidth = 0.2, linetype = "solid", color = "gray90")
      )
  }

  # ---y axis scaling
  aplot <- aplot + scale_y_continuous(
    limits = y_limits,
    breaks = y_major_breaks,
    minor_breaks = y_minor_breaks,
    labels = function(x) {format(x,nsmall = y_decimals,scientific=y_scientific)}
  )

  # -------------------rotate/size tic labels--------------------
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

  # -----------------------hide x/y axis tics?----------------------
  if(hide_x_tics){
    aplot <- aplot +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }
  if(hide_y_tics){
    aplot <- aplot +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }

  # If aes_x is NULL then remove x axis title, tic text, ticks
  if(aes_x_is_undefined){
    if(!do_coord_flip){
      aplot <- aplot +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    }else if(do_coord_flip){
      aplot <- aplot +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    }
  }
  # ------------------Are we flipping the boxplot?---------------------
  if(do_coord_flip){
    aplot <- aplot + coord_flip()
  }

  # -------------------legend related parameters---------------------------
  if(!show_legend){
    aplot <- aplot +
      theme(legend.position = "none")
  }else {
    aplot <- aplot +
      theme(
        legend.position = legend_pos,
        legend.key = element_rect(fill = legend_key_backgrd),
        legend.key.width = unit(legend_key_width, "cm"),
        legend.key.height = unit(legend_key_height, "cm")
      )
  }

  # -----------------save the plot as a png file?------------------
  if(!is.null(png_file_path)){
    grDevices::png(
      filename = png_file_path,
      width = png_width_height[[1]],
      height = png_width_height[[2]]
    )
    print(aplot)
    grDevices::dev.off()
  }

  return(aplot)
}
