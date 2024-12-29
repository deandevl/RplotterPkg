#' @title percentile_table
#'
#' @description Function locates values corresponding to percentiles in a vector of numeric data.
#'   Function displays a table with lower/upper locations, values,
#'   average and spread between lower/upper from median to one hundred and twenty-eighth percentiles.
#'   The function returns a list with a viewable gt table along with the data.table
#'   containing the table's values.
#'
#'   The "label" column of the table correspond to the following percentiles:
#'
#' \enumerate{
#'  \item "M" = Half (i.e. the median)
#'  \item "F" = Fourth (i.e. quartiles 25/75)
#'  \item "E" = Eighth
#'  \item "D" = Sixteenth
#'  \item "C" = Thirty-secondth
#'  \item "B" = Sixty-fourth
#'  \item "A" = One hundred and twenty-eighth
#' }
#'
#' @param vals A required vector of numeric values
#' @param na_rm A logical which if TRUE removes NA values before
#'   the calculations
#' @param title A string that sets the table's title. The string can contain R markdown/html style syntax.
#' @param table_width A numeric that sets the overall table width in pixels.
#'
#' @return A named list with a gt object and a data.table with percentile values contained in the display table.
#'
#' @examples
#' library(gt)
#' library(data.table)
#' library(RplotterPkg)
#'
#' set.seed(12345)
#' random_vals <- stats::rnorm(n = 1000, mean = 20, sd = 4)
#' percentile_random_lst <- RplotterPkg::percentile_table(
#'   vals = random_vals
#' )
#' percentile_random_lst$table_gt
#'
#' @import data.table
#' @import gt
#' @importFrom stats na.omit
#'
#' @export
percentile_table <- function(
  vals = NULL,
  na_rm = TRUE,
  title = NULL,
  table_width = NULL
){
  if(is.null(vals)){
    stop("The 'vals' parameter must be a numeric vector")
  }

  if(na_rm){
    vals <- na.omit(vals)
  }
  vals <- sort(vals)
  n <- length(vals)
  cpos <- n
  depth <- c()
  percent_letters <- c("M", "F", "E", "D", "C", "B", "A")
  while (cpos > 1 & length(depth) < 7) {
    cpos <- (floor(cpos) + 1) / 2
    if (cpos != 1.5) {
      depth <- c(depth, cpos)
    }
  }

  lo <- (vals[floor(depth)] + vals[ceiling(depth)]) / 2
  hi <- (vals[floor(n + 1 - depth)] + vals[ceiling(n + 1 - depth)])/2
  mids <- (lo + hi)/2
  spreads <- hi - lo
  label <- percent_letters[1:length(depth)]

  dt <- data.table(label, depth, lo, hi, mids, spreads)

  table_gt <- RplotterPkg::create_table(
    df = dt,
    title = title,
    container_width_px = table_width
  )
  return(list(dt = dt, table_gt = table_gt))
}
