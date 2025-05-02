#' Test Uniformity of Circular Residuals
#'
#' Applies Rayleigh, Kuiper, and Watson \eqn{U^2} tests to assess whether residuals
#' are uniformly distributed on the circle. These tests are commonly used to detect
#' lack of fit in circular models.
#'
#' @param resids Numeric vector of residuals (in radians).
#'
#' @return A list with components:
#' \describe{
#'   \item{rayleigh}{Result of the Rayleigh test.}
#'   \item{kuiper}{Result of the Kuiper test.}
#'   \item{watson}{Result of the Watson \eqn{U^2} test.}
#' }
#' 
#' @importFrom stats residuals runif
#' @importFrom circular circular rvonmises
#' @importFrom circular rayleigh.test kuiper.test watson.test
#'
#' @examples
#' resids <- runif(100, -pi, pi)
#' test_residuals_uniformity(resids)
#'
#' @export
test_residuals_uniformity <- function(resids) {
  if (!requireNamespace("circular", quietly = TRUE)) {
    stop("Package 'circular' is required for this function.")
  }
  
  resids_circ <- circular(resids, units = "radians", modulo = "2pi")
  
  rayleigh_result <- rayleigh.test(resids_circ)
  kuiper_result <- kuiper.test(resids_circ)
  watson_result <- watson.test(resids_circ)
  
  return(list(
    rayleigh = rayleigh_result,
    kuiper = kuiper_result,
    watson = watson_result
  ))
}
