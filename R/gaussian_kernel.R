#' Gaussian Kernel Function
#'
#' Computes the Gaussian kernel weight for a given input value.
#' Commonly used in nonparametric regression for continuous covariates.
#'
#' @param u A numeric value (typically standardized: \code{(x - x0)/h}).
#'
#' @return A numeric value representing the Gaussian kernel weight at \code{u}.
#'
#' @examples
#' # Maximum at the center (u = 0)
#' gaussian_kernel(0)
#'
#' # Decaying weight as |u| increases
#' gaussian_kernel(1)
#' gaussian_kernel(-1)
#'
#' # Very low influence for distant points
#' gaussian_kernel(3)
#' gaussian_kernel(-3)
#'
#' @export
gaussian_kernel <- function(u) {
  (1 / sqrt(2 * pi)) * exp(-0.5 * u^2)
}
