#' Compute product kernel weights for mixed predictors
#'
#' Computes normalized product-kernel weights for a given target point 
#' \eqn{(x_0, z_0)} using both continuous and categorical predictors.
#' Continuous predictors are smoothed using a multivariate product of 
#' univariate kernel functions; categorical predictors use separate kernel 
#' functions that may account for nominal or ordinal structure.
#'
#' This version handles unmatched levels gracefully when using ordinal or nominal kernels, 
#' and returns uniform weights if all computed weights are zero (to avoid division by zero).
#'
#' @param x0 Numeric vector of length \eqn{k}, containing the continuous predictors at the target point.
#' @param X Numeric matrix of size \eqn{n \times k}, where each row corresponds to a sample of continuous predictors.
#' @param z0 Vector of length \eqn{p}, containing the categorical predictors at the target point.
#' @param Z Data frame or matrix of size \eqn{n \times p}, containing the categorical predictors for each sample.
#' @param h Numeric vector of length \eqn{k}, with bandwidths for continuous variables.
#' @param lambda Numeric vector of length \eqn{p}, with smoothing parameters for categorical variables.
#' @param kernel_continuous Univariate kernel function to be applied to each continuous variable (e.g., \code{dnorm}).
#' @param kernel_categorical Kernel function for categorical variables (e.g., \code{aitchison_aitken_kernel} or \code{ordinal_kernel}).
#' @param levels_list Optional list of levels for each categorical variable; required if using ordinal kernels.
#'
#' @return A numeric vector of length \code{n} with the normalized kernel weights.
#'
#' @examples
#' # Simulate example data
#' set.seed(123)
#' X <- matrix(runif(20), ncol = 1)
#' Z <- data.frame(group = sample(c("Low", "Medium", "High"), 20, replace = TRUE))
#' x0 <- 0.5
#' z0 <- "Medium"
#' h <- 0.1
#' lambda <- 0.3
#'
#' # Define Gaussian kernel
#' gaussian_kernel <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)
#'
#' # Define ordinal kernel
#' ordinal_kernel <- function(z, zi, lambda, levels) {
#'   z <- as.character(z)
#'   zi <- as.character(zi)
#'   d <- abs(match(z, levels) - match(zi, levels))
#'   return((1 - lambda)^d)
#' }
#'
#' # Specify level ordering for ordinal kernel
#' levels_list <- list(c("Low", "Medium", "High"))
#'
#' # Compute weights
#' kernel_weights(
#'   x0 = x0, X = X,
#'   z0 = z0, Z = Z,
#'   h = h, lambda = lambda,
#'   kernel_continuous = gaussian_kernel,
#'   kernel_categorical = ordinal_kernel,
#'   levels_list = levels_list
#' )
#'
#' @export
kernel_weights <- function(x0, X, z0, Z, 
                           h, lambda, 
                           kernel_continuous, kernel_categorical,
                           levels_list = NULL) {
  
  # --- Validations (lightweight but safe) ---
  n <- nrow(X)
  k <- length(x0)
  p <- length(z0)
  
  if (length(h) != k) stop("Length of 'h' must match number of continuous predictors.")
  if (length(lambda) != p) stop("Length of 'lambda' must match number of categorical predictors.")
  if (nrow(Z) != n) stop("X and Z must have the same number of rows.")
  
  weights <- numeric(n)
  
  for (i in 1:n) {
    # Continuous part
    wx <- 1
    for (j in 1:k) {
      u <- (x0[j] - X[i, j]) / h[j]
      wx <- wx * kernel_continuous(u)
    }
    
    # Categorical part
    wz <- 1
    for (l in 1:p) {
      z0l <- as.character(z0[l])
      Zil <- as.character(Z[i, l])
      
      if (!is.null(levels_list)) {
        levs <- levels_list[[l]]
        if (!(z0l %in% levs) || !(Zil %in% levs)) {
          warning(sprintf("Level '%s' or '%s' not found in levels_list[[%d]]: setting weight = 0", z0l, Zil, l))
          wz <- 0
          break
        }
        wz <- wz * kernel_categorical(z0l, Zil, lambda[l], levels = levs)
      } else {
        wz <- wz * kernel_categorical(z0l, Zil, lambda[l])
      }
    }
    
    weights[i] <- wx * wz
  }
  
  wsum <- sum(weights)
  if (wsum == 0) {
    warning("All kernel weights are zero. Returning uniform weights.")
    return(rep(1 / n, n))
  }
  
  return(weights / wsum)
}
