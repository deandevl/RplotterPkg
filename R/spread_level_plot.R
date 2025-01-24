#' @title spread_level_plot
#'
#' @description Function produces a scatter plot of median versus spread across a variable's
#'   factor levels.
#'
#'   The spread is defined as the difference between the 75th and 25th quartiles.
#'  Function returns a named list with a data.table of each level's median, quartile values,
#'   and a ggplot2 scatter plot with medians along the x axis and spreads along the y axis.
#'
#' @param df The required target data frame with a measure variable and a factor variable
#'   with multiple levels.
#' @param meas_var A required string that names the measure variable from 'df'.
#' @param factor_var A required string that names the factor variable from 'df'.
#' @param plot_line_fit A logical which if \code{TRUE} plots a line fit between median and spread values.
#' @param plot_log10 A logical which if \code{TRUE} will plot log10 values of median versus spread
#'   instead of raw values.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param caption A string that sets the plot caption
#' @param center_titles A logical which if \code{TRUE} centers both the 'title' and 'subtitle'.
#' @param x_title A string that sets the x axis title. If \code{NULL} then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If \code{NULL} then the y axis title does not appear.
#' @param x_limits A numeric 2 element vector that sets the minimum
#'  and maximum for the x axis.
#' @param x_major_breaks A numeric vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param label_pts A logical which if \code{TRUE} will label the plot points.
#'
#' @return A list object with a data.table and ggplot2 scatter plot.
#'
#' @examples
#' library(here)
#' library(data.table)
#' library(ggplot2)
#' library(RplotterPkg)
#'
#' spread_level_lst <- RplotterPkg::spread_level_plot(
#'   df = RplotterPkg::homeruns_2000,
#'   meas_var = "HOMERUNS",
#'   factor_var = "YEARS",
#'   x_title = "Log Median",
#'   y_title = "Log Spread"
#' )
#' spread_level_lst$scatter_plot
#'
#' @import ggplot2
#' @importFrom data.table as.data.table
#' @importFrom stats lm
#'
#' @export
spread_level_plot <- function(
  df,
  meas_var,
  factor_var,
  plot_line_fit = TRUE,
  plot_log10 = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  center_titles = FALSE,
  x_title = "Median",
  y_title = "Spread",
  x_limits = NULL,
  x_major_breaks = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  label_pts = TRUE
){
  q_lo <- q_hi <- M <- fitted_val <- . <- NULL

  dt <- data.table::as.data.table(df)

  ans_dt <- dt[,.(
    M = stats::median(get(meas_var)),
    q_lo = stats::quantile(get(meas_var), probs = 0.25),
    q_hi = stats::quantile(get(meas_var), probs = 0.75)),
    by = mget(factor_var)]

  ans_dt[, `:=`(
    spread = q_hi - q_lo,
    median_log = log10(M),
    spread_log = log10(q_hi - q_lo)
  )]

  spread_level_lm <- lm(spread_log ~ 0 + median_log, data = ans_dt)
  ans_dt[, fitted_val := spread_level_lm$fitted.values]

  aes_x <- "M"
  aes_y <- "spread"
  aes_label <- NULL
  if(plot_log10){
    aes_x <- "median_log"
    aes_y <- "spread_log"
  }
  if(label_pts){
    aes_label = factor_var
  }

  scatter_plot <- RplotterPkg::create_scatter_plot(
    df = ans_dt,
    aes_x = aes_x,
    aes_y = aes_y,
    aes_label = aes_label,
    aes_label_size = 3,
    aes_label_nudge_y = 0.015,
    title = title,
    subtitle = subtitle,
    caption = caption,
    center_titles = center_titles,
    x_title = x_title,
    y_title = y_title,
    x_limits = x_limits,
    x_major_breaks = x_major_breaks,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks
  )

  if(plot_line_fit){
    scatter_plot <- scatter_plot + geom_line(aes(y = fitted_val), color = "red")
  }

  return(
    list(
      df = ans_dt,
      spread_level_lm = spread_level_lm,
      scatter_plot = scatter_plot
    )
  )
}
