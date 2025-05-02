#' Nadaraya-Watson Circular Regression Estimator with Mixed Predictors
#'
#' Computes the circular nonparametric regression estimate at a given point
#' using a product-kernel Nadaraya-Watson estimator with both continuous and categorical covariates.
#'
#' @param x0 Numeric vector of length \code{k}, values of the continuous predictors at the target point.
#' @param z0 Vector of length \code{p}, values of the categorical predictors at the target point.
#' @param X Numeric matrix of size \code{n x k} with the continuous predictors.
#' @param Z Data frame or matrix of size \code{n x p} with the categorical predictors.
#' @param theta Numeric vector of length \code{n}, circular responses in radians.
#' @param h Numeric vector of bandwidths for the continuous predictors (length \code{k}).
#' @param lambda Numeric vector of smoothing parameters for the categorical predictors (length \code{p}).
#' @param kernel_continuous Function to use as kernel for continuous covariates (e.g., \code{gaussian_kernel}).
#' @param kernel_categorical Function to use as kernel for categorical covariates (e.g., \code{aitchison_aitken_kernel}, \code{ordinal_kernel}).
#' @param kernel_levels Optional list of factor levels for each categorical variable (only needed for ordinal kernels).
#'
#' @return A numeric scalar: estimated circular regression value at \code{(x0, z0)}, in radians.
#'
#' @importFrom stats runif
#'
#' @examples
#' set.seed(1)
#' X <- matrix(runif(30), ncol = 1)
#' Z <- data.frame(group = sample(c("A", "B", "C"), 30, replace = TRUE))
#' theta <- runif(30, 0, 2 * pi)
#' x0 <- c(0.5)
#' z0 <- c("B")
#' h <- 0.2
#' lambda <- 0.3
#'
#' gaussian_kernel <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)
#'
#' aitchison_aitken_kernel <- function(z, zi, lambda, levels = NULL) {
#'   z <- as.character(z)
#'   zi <- as.character(zi)
#'   if (is.null(levels)) levels <- sort(unique(c(z, zi)))
#'   if (!(z %in% levels) || !(zi %in% levels)) return(0)
#'   k <- length(levels)
#'   if (z == zi) return(1 - lambda) else return(lambda / (k - 1))
#' }
#'
#' nw_circular_mixed(
#'   x0, z0, X, Z, theta, h, lambda,
#'   kernel_continuous = gaussian_kernel,
#'   kernel_categorical = aitchison_aitken_kernel,
#'   kernel_levels = list(c("A", "B", "C"))
#' )
#'
#' @export
nw_circular_mixed <- function(x0, z0, X, Z, theta, h, lambda,
                              kernel_continuous,
                              kernel_categorical,
                              kernel_levels = NULL) {
  
  n <- length(theta)
  k <- if (!is.null(X)) ncol(X) else 0
  p <- if (!is.null(Z)) ncol(Z) else 0
  
  if (length(h) != k) stop("Length of 'h' must match number of continuous predictors.")
  if (length(lambda) != p) stop("Length of 'lambda' must match number of categorical predictors.")
  if (!is.null(kernel_levels) && length(kernel_levels) != p) {
    stop("kernel_levels must be a list of length equal to the number of categorical predictors.")
  }
  
  weights <- numeric(n)
  
  for (i in 1:n) {
    # Continuous component
    w_cont <- if (k > 0) {
      prod(sapply(1:k, function(j) {
        u <- (x0[j] - X[i, j]) / h[j]
        kernel_continuous(u)
      }))
    } else 1
    
    # Categorical component
    w_cat <- if (p > 0) {
      prod(sapply(1:p, function(l) {
        z0l <- as.character(z0[l])
        Zil <- as.character(Z[i, l])
        levs <- if (!is.null(kernel_levels)) kernel_levels[[l]] else unique(c(Z[[l]], z0l))
        if (!(z0l %in% levs) || !(Zil %in% levs)) return(0)
        kernel_categorical(z0l, Zil, lambda[l], levels = levs)
      }))
    } else 1
    
    weights[i] <- w_cont * w_cat
  }
  
  w_sum <- sum(weights)
  if (w_sum == 0 || anyNA(weights)) {
    warning("All weights are zero or contain NA. Returning NA.")
    return(NA)
  }
  
  weights <- weights / w_sum
  sin_part <- sum(weights * sin(theta))
  cos_part <- sum(weights * cos(theta))
  
  return(atan2(sin_part, cos_part) %% (2 * pi))
}
