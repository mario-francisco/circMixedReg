#' Plot Circular Histogram of Residuals
#'
#' Displays a circular histogram (rose diagram) of the residuals.
#' This is useful to visually assess the distribution of residuals
#' in circular regression models.
#'
#' @param resids Numeric vector of residuals (in radians).
#' @param bins Integer. Number of bins in the circular histogram. Default is 16.
#' @param main Character. Title of the plot. Default is \code{"Circular Histogram of Residuals"}.
#'
#' @return A circular histogram plot created with \code{rose.diag()}.
#' 
#' @importFrom stats residuals sd
#' @importFrom circular rose.diag
#'
#' @examples
#' resids <- circular::circular(rnorm(100, mean = 0, sd = 1), units = "radians")
#' plot_residuals_histogram(resids, bins = 12, main = "Example: Residuals")
#'
#' @export
plot_residuals_histogram <- function(resids, bins = 16, main = "Circular Histogram of Residuals") {
  resids_circ <- circular(resids, units = "radians", modulo = "2pi")
  rose.diag(resids_circ, bins = bins, main = main, shrink = 1)
}
