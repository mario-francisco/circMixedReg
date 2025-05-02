#' Compute CMASE_obs by Group
#'
#' Computes the observational Circular Mean Average Squared Error (CMASE_obs) separately
#' for each level of a categorical grouping variable.
#'
#' This function is useful to evaluate the performance of a circular regression model
#' across different subgroups (e.g., sensory conditions or experimental treatments).
#'
#' @param theta Vector of observed circular responses (in radians). Can be numeric or of class \code{circular}.
#' @param x Matrix or data frame of continuous predictors (each row = one observation).
#' @param z Vector or factor of categorical predictor used to define the groups.
#' @param fit_fun A user-defined function that takes \code{x} and \code{z} as inputs and returns predicted values.
#' @param condition_levels Character vector with the levels of \code{z} to evaluate.
#'
#' @return A named numeric vector of CMASE_obs values, one for each group in \code{condition_levels}.
#' 
#' @importFrom circular circular
#'
#' @examples
#' # Simulated example
#' set.seed(123)
#' n <- 60
#' x <- matrix(runif(n), ncol = 1)
#' z <- factor(rep(c("A", "B", "C"), each = 20))
#' mu_fun <- function(x, z) {
#'   if (z == "A") return(pi * x)
#'   if (z == "B") return(pi / 2 * x + pi / 4)
#'   return(pi * (1 - x))
#' }
#' theta <- sapply(1:n, function(i) (mu_fun(x[i], z[i]) + circular::rvonmises(1, 0, 3)) %% (2 * pi))
#' fit_fun <- function(xnew, znew) sapply(1:nrow(xnew), function(i) mu_fun(xnew[i], znew[i]))
#' compute_CMASE_obs_by_group(theta, x, z, fit_fun, condition_levels = c("A", "B", "C"))
#'
#' @export
compute_CMASE_obs_by_group <- function(theta, x, z, fit_fun, condition_levels) {
  theta <- circular(theta, units = "radians", modulo = "2pi")
  x <- as.matrix(x)  # Ensures correct subsetting even with one predictor
  
  cmase_by_group <- numeric(length(condition_levels))
  names(cmase_by_group) <- condition_levels
  
  for (i in seq_along(condition_levels)) {
    group_idx <- which(z == condition_levels[i])
    theta_group <- theta[group_idx]
    x_group <- x[group_idx, , drop = FALSE]
    z_group <- z[group_idx]
    
    m_hat_group <- fit_fun(x_group, z_group)
    m_hat_group <- circular(m_hat_group, units = "radians", modulo = "2pi")
    
    cmase_by_group[i] <- mean(1 - cos(theta_group - m_hat_group), na.rm = TRUE)
  }
  
  return(cmase_by_group)
}
