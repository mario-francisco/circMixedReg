#' Local Linear Circular Regression Estimator with Mixed Predictors
#'
#' Computes the nonparametric circular regression estimate at a given point
#' using a local linear product-kernel estimator with both continuous and categorical predictors.
#'
#' @param x0 Numeric vector of length \code{k} with the values of the continuous predictors at the target point.
#' @param z0 Vector of length \code{p} with the values of the categorical predictors at the target point.
#' @param X Matrix of continuous predictors (\code{n x k}).
#' @param Z Data frame or matrix of categorical predictors (\code{n x p}).
#' @param theta Vector of circular responses (in radians).
#' @param h Vector of bandwidths for the continuous variables (length \code{k}).
#' @param lambda Vector of smoothing parameters for the categorical variables (length \code{p}).
#' @param kernel_continuous Function to use as kernel for continuous predictors (e.g., \code{gaussian_kernel}).
#' @param kernel_categorical Function to use as kernel for categorical predictors (e.g., \code{aitchison_aitken_kernel} or \code{ordinal_kernel}).
#' @param kernel_levels Optional list of levels for each categorical variable (needed by some categorical kernels).
#'
#' @return A numeric scalar: estimated circular regression value at \code{(x0, z0)} in radians.
#'
#' @importFrom stats runif
#'
#' @examples
#' set.seed(123)
#' n <- 30
#' X <- matrix(runif(n), ncol = 1)
#' Z <- data.frame(group = sample(c("A", "B", "C"), n, replace = TRUE))
#' theta <- 2 * pi * X[,1] + rnorm(n, sd = 0.2)
#'
#' x0 <- 0.5
#' z0 <- "B"
#' h <- 0.2
#' lambda <- 0.3
#' levels_list <- list(c("A", "B", "C"))
#'
#' gaussian_kernel <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)
#' ordinal_kernel <- function(z, zi, lambda, levels) {
#'   d <- abs(match(z, levels) - match(zi, levels))
#'   (1 - lambda)^d
#' }
#'
#' ll_circular_mixed(
#'   x0 = x0, z0 = z0,
#'   X = X, Z = Z,
#'   theta = theta,
#'   h = h, lambda = lambda,
#'   kernel_continuous = gaussian_kernel,
#'   kernel_categorical = ordinal_kernel,
#'   kernel_levels = levels_list
#' )
#'
#' @export
ll_circular_mixed <- function(x0, z0, X, Z, theta, h, lambda,
                              kernel_continuous = gaussian_kernel,
                              kernel_categorical = aitchison_aitken_kernel,
                              kernel_levels = NULL) {
  
  n <- length(theta)
  k <- if (!is.null(X)) ncol(X) else 0
  p <- if (!is.null(Z)) ncol(Z) else 0
  
  if (length(h) != k) stop("Length of 'h' must match number of continuous predictors.")
  if (length(lambda) != p) stop("Length of 'lambda' must match number of categorical predictors.")
  if (!is.null(kernel_levels) && length(kernel_levels) != p) {
    stop("kernel_levels must be a list of length equal to the number of categorical predictors.")
  }
  
  weights <- LL_kernel_weights(
    x0 = x0, X = X,
    z0 = z0, Z = Z,
    h = h, lambda = lambda,
    kernel_continuous = kernel_continuous,
    kernel_categorical = kernel_categorical,
    levels_list = kernel_levels
  )
  
  w_sum <- sum(weights)
  if (w_sum == 0 || anyNA(weights)) {
    warning("All weights are zero or contain NA. Returning NA.")
    return(NA)
  }
  
  sin_part <- sum(weights * sin(theta))
  cos_part <- sum(weights * cos(theta))
  
  return(atan2(sin_part, cos_part) %% (2 * pi))
}
