

#' Title Quadratic Function for calculating Spruce Height
#'
#' @param x BHDiameter of Spruce
#'
#' @return Height of Spruce
#' @export
#'
#' @examples myPlot(x = 1:10)
myPlot <- function(x){
  0.86089580 +1.46959217*x  -0.02745726*x^2
}
