#' Epanechnikov Kernel Function
#'
#' Computes the Epanechnikov kernel weight for a given input value.
#' This kernel is optimal in the mean square error sense for symmetric distributions and has compact support.
#'
#' @param u A numeric value (typically standardized: \code{(x - x0)/h}).
#'
#' @return A numeric value representing the Epanechnikov kernel weight at \code{u}, or 0 if \code{|u| > 1}.
#'
#' @examples
#' # Maximum at the center (u = 0)
#' epanechnikov_kernel(0)
#'
#' # Weight decreases quadratically with distance
#' epanechnikov_kernel(0.5)
#'
#' # Zero weight outside support
#' epanechnikov_kernel(1.2)
#' epanechnikov_kernel(-1.5)
#'
#' @export
epanechnikov_kernel <- function(u) {
  if (abs(u) <= 1) {
    0.75 * (1 - u^2)
  } else {
    0
  }
}
