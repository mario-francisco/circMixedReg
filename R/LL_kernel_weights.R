#' Compute local linear kernel weights for circular regression with mixed predictors
#'
#' Computes normalized local linear kernel weights for a given target point 
#' \eqn{(x_0, z_0)}, using continuous and categorical predictors. The estimator 
#' applies a local linear correction to the continuous covariates and standard 
#' kernel smoothing to the categorical covariates.
#'
#' @param x0 Numeric vector of length \eqn{k}, continuous predictors at the target point.
#' @param X Numeric matrix of size \eqn{n \times k}, continuous predictors for all observations.
#' @param z0 Vector of length \eqn{p}, categorical predictors at the target point.
#' @param Z Data frame or matrix of size \eqn{n \times p}, categorical predictors for all observations.
#' @param h Numeric vector of bandwidths for the continuous variables.
#' @param lambda Numeric vector of smoothing parameters for the categorical variables.
#' @param kernel_continuous Univariate kernel function for continuous variables (e.g., \code{dnorm}).
#' @param kernel_categorical Kernel function for categorical variables (e.g., \code{ordinal_kernel}).
#' @param levels_list Optional list of level sets for each categorical variable (required for ordinal kernels).
#'
#' @return A numeric vector of length \code{n} with the normalized local linear kernel weights.
#'
#' @examples
#' set.seed(42)
#' X <- matrix(runif(30), ncol = 1)
#' Z <- data.frame(condition = sample(c("Low", "Medium", "High"), 30, replace = TRUE))
#' x0 <- 0.5
#' z0 <- "Medium"
#' h <- 0.2
#' lambda <- 0.3
#'
#' gaussian_kernel <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)
#'
#' ordinal_kernel <- function(z, zi, lambda, levels) {
#'   z <- as.character(z)
#'   zi <- as.character(zi)
#'   d <- abs(match(z, levels) - match(zi, levels))
#'   return((1 - lambda)^d)
#' }
#'
#' levels_list <- list(c("Low", "Medium", "High"))
#'
#' LL_kernel_weights(
#'   x0 = x0, X = X, z0 = z0, Z = Z,
#'   h = h, lambda = lambda,
#'   kernel_continuous = gaussian_kernel,
#'   kernel_categorical = ordinal_kernel,
#'   levels_list = levels_list
#' )
#'
#' @export
LL_kernel_weights <- function(x0, X, z0, Z,
                              h, lambda,
                              kernel_continuous, kernel_categorical,
                              levels_list = NULL) {
  
  # --- Validations ---
  n <- nrow(X)
  k <- length(x0)
  p <- length(z0)
  
  if (length(h) != k) stop("Length of 'h' must match number of continuous predictors.")
  if (length(lambda) != p) stop("Length of 'lambda' must match number of categorical predictors.")
  if (nrow(Z) != n) stop("X and Z must have the same number of rows.")
  
  K_vals <- numeric(n)
  L_vals <- numeric(n)
  diff_X <- matrix(0, n, k)
  
  for (i in 1:n) {
    # Continuous part
    wx <- 1
    for (j in 1:k) {
      u <- (x0[j] - X[i, j]) / h[j]
      wx <- wx * kernel_continuous(u)
      diff_X[i, j] <- X[i, j] - x0[j]
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
    
    K_vals[i] <- wx
    L_vals[i] <- wz
  }
  
  weights_product <- K_vals * L_vals
  
  # Local linear correction
  S0 <- sum(weights_product)
  S1 <- colSums(weights_product * diff_X)
  S2 <- sum(weights_product * rowSums(diff_X^2))
  denom <- S0 * S2 - sum(S1^2)
  if (denom == 0) denom <- 1e-8  # avoid division by zero
  
  w_tilde <- numeric(n)
  for (i in 1:n) {
    correction <- S2 - sum(diff_X[i, ] * S1)
    w_tilde[i] <- weights_product[i] * correction / denom
  }
  
  # Normalize
  w_sum <- sum(w_tilde)
  if (w_sum == 0) {
    warning("All local linear weights are zero. Returning uniform weights.")
    return(rep(1 / n, n))
  }
  
  return(w_tilde / w_sum)
}
