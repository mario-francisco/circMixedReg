#' Compute Circular Mean Average Squared Error (Observational)
#'
#' Computes the empirical circular mean squared error between observed responses
#' and predicted values. This corresponds to the observational version of CMASE
#' (Circular Mean Average Squared Error) as defined in the article, and is the
#' circular analogue of the usual MSE.
#'
#' @param theta Numeric or circular vector of observed responses (in radians).
#' @param theta_hat Numeric or circular vector of predicted responses (in radians).
#'
#' @return A scalar: the mean of the squared circular errors, i.e., CMASE_obs.
#' 
#' @importFrom circular circular 
#' 
#' @examples
#' theta <- circular::circular(runif(100, 0, 2 * pi))
#' theta_hat <- circular::circular(runif(100, 0, 2 * pi))
#' compute_CMASE_obs(theta, theta_hat)
#'
#' @export
compute_CMASE_obs <- function(theta, theta_hat) {
  theta <- circular(theta, units = "radians", modulo = "2pi")
  theta_hat <- circular(theta_hat, units = "radians", modulo = "2pi")
  mean(1 - cos(theta - theta_hat), na.rm = TRUE)
}
