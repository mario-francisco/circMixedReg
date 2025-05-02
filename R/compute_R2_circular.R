#' Compute Circular Coefficient of Determination R^2
#'
#' Computes the circular analogue of the coefficient of determination,
#' based on the cosine distance between observed and predicted angular responses.
#'
#' The formula used is:
#' \deqn{
#'   R^2_{\text{circ}} = 1 - \frac{\sum (1 - \cos(\theta_i - \hat{\theta}_i))}{\sum (1 - \cos(\theta_i - \bar{\theta}))}
#' }{
#'   R^2_circ = 1 - SSE / SST
#' }
#' where \eqn{\bar{\theta}} is the circular mean of the observed angles.
#'
#' @param theta Vector of observed circular responses (in radians).
#' @param theta_hat Vector of predicted circular responses (in radians).
#'
#' @return A scalar representing the circular coefficient of determination.
#' 
#' @importFrom circular circular mean.circular
#'
#' @examples
#' set.seed(123)
#' theta <- circular::circular(runif(100, 0, 2 * pi))
#' theta_hat <- circular::circular(runif(100, 0, 2 * pi))
#' compute_R2_circular(theta, theta_hat)
#'
#' @export
compute_R2_circular <- function(theta, theta_hat) {
  theta <- circular(theta, units = "radians", modulo = "2pi")
  theta_hat <- circular(theta_hat, units = "radians", modulo = "2pi")
  
  theta_bar <- mean.circular(theta)
  
  SSE <- sum(1 - cos(theta - theta_hat), na.rm = TRUE)
  SST <- sum(1 - cos(theta - theta_bar), na.rm = TRUE)
  
  R2 <- 1 - SSE / SST
  return(R2)
}
