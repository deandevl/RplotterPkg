#' get_grob_component
#'
#' Function retrieves a grob component from a ggplot2 plot
#'  object.
#'
#' @param a_plot A ggplot2 plot object
#' @param component_name A string that sets the components name
#'
#' @return A grob object
#'
#' @author Rick Dean
#'
#' @export
get_grob_component <- function(a_plot, component_name){
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
