#' @title get_grob_component
#'
#' @description Function retrieves a grob component from a ggplot2 plot
#'  object.
#'
#' @param a_plot A required ggplot2 plot object
#' @param component_name A required string that sets the components name
#'
#' @return A list of grob objects
#'
#' @examples
#' library(ggplot2)
#' library(ggplotify)
#' library(grid)
#' library(RplotterPkg)
#'
#' mtcars_plot <- ggplot2::ggplot(
#'   data = datasets::mtcars,
#' ) +
#' ggplot2::geom_point(aes(x = mpg, y = wt, color = cyl), size = 3)
#'
#' legend_right <- RplotterPkg::get_grob_component(
#'   a_plot = mtcars_plot,
#'   component_name = "guide-box-right"
#' )
#'
#' @importFrom ggplotify as.grob
#' @importFrom grid is.grob
#'
#' @export
get_grob_component <- function(
  a_plot = NULL,
  component_name = NULL
){

  if(is.null(a_plot) | is.null(component_name)){
    stop("Both parameters 'a_plot' and 'component_name' must be submitted")
  }

  plot_gt <- a_plot
  if(!grid::is.grob(plot_gt)){
    plot_gt <- ggplotify::as.grob(a_plot)
  }
  gt_names <- plot_gt$layout$name
  gt_grobs <- plot_gt$grobs
  grob_idx <- which(grepl(component_name,gt_names))
  matched_grob <- NULL

  if(length(grob_idx) != 0){
    #matched_grob <- gt_grobs[[grob_idx]]
    matched_grob <- gt_grobs[[grob_idx[1]]]
  }
  return(matched_grob)
}
