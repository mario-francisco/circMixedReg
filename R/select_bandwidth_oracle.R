#' Oracle Bandwidth Selector for Circular Regression
#'
#' Selects the optimal bandwidths (for continuous and categorical predictors)
#' by minimizing the true Circular Mean Integrated Squared Error (CMISE)
#' using a known regression function. Useful for simulation benchmarking.
#'
#' Supports both Nadaraya-Watson ("NW") and local linear ("LL") estimators.
#'
#' @param x_grid Matrix or data frame of evaluation points (n_eval × k).
#' @param m_true Vector of true regression function values at \code{x_grid}.
#' @param X Matrix of continuous predictors used in estimation (n × k).
#' @param Z Data frame or matrix of categorical predictors (n × p). Can be NULL.
#' @param theta Vector of circular responses (in radians).
#' @param h_grid Optional list of candidate bandwidths for continuous variables. If NULL, a default grid is used.
#' @param lambda_grid Optional list of candidate smoothing parameters for categorical variables. If NULL, a default grid is used.
#' @param kernel_continuous Kernel function for continuous predictors.
#' @param kernel_categorical Kernel function for categorical predictors.
#' @param kernel_levels Optional list of level vectors (only needed for kernels like ordinal).
#' @param estimator Character string: either \code{"NW"} (default) or \code{"LL"}.
#' @param B Number of Monte Carlo replicates for CMISE estimation. Default is 1.
#'
#' @return A list with:
#' \describe{
#'   \item{h_oracle}{Optimal bandwidths for continuous variables.}
#'   \item{lambda_oracle}{Optimal smoothing parameters for categorical variables.}
#'   \item{cmise_oracle}{Minimum estimated CMISE over the grid.}
#' }
#'
#' @importFrom circular circular
#' @importFrom stats sd
#'
#' @examples
#' set.seed(123)
#' n <- 30
#' x <- runif(n)
#' z <- sample(c("A", "B", "C"), n, replace = TRUE)
#' f_true <- function(x, z) {
#'   if (z == "A") return(pi * x)
#'   if (z == "B") return(pi / 2 * x + pi / 4)
#'   return(pi * (1 - x))
#' }
#' theta <- sapply(1:n, function(i) (f_true(x[i], z[i]) + circular::rvonmises(1, 0, 5)) %% (2 * pi))
#' 
#' data <- data.frame(x = x, z = z, theta = theta)
#' x_grid <- matrix(seq(0, 1, length.out = 25), ncol = 1)
#' m_true <- sapply(x_grid, function(xi) f_true(xi, "B"))  # Fix z = "B"
#' 
#' result <- select_bandwidth_oracle(
#'   x_grid = x_grid,
#'   m_true = m_true,
#'   X = as.matrix(data$x),
#'   Z = data["z"],
#'   theta = data$theta,
#'   estimator = "NW",
#'   kernel_continuous = gaussian_kernel,
#'   kernel_categorical = aitchison_aitken_kernel,
#'   kernel_levels = list(z = c("A", "B", "C")),
#'   B = 1
#' )
#' 
#' result$h_oracle
#' result$lambda_oracle
#' result$cmise_oracle
#'
#' @export
select_bandwidth_oracle <- function(x_grid, m_true, X, Z, theta,
                                    h_grid = NULL, lambda_grid = NULL,
                                    kernel_continuous = gaussian_kernel,
                                    kernel_categorical = aitchison_aitken_kernel,
                                    kernel_levels = NULL,
                                    estimator = "NW",
                                    B = 1) {
  estimator <- match.arg(estimator, choices = c("NW", "LL"))
  theta <- circular::circular(theta, units = "radians", modulo = "2pi")
  
  k <- if (!is.null(X)) ncol(X) else 0
  p <- if (!is.null(Z)) ncol(Z) else 0
  
  # Generate default bandwidth grids if missing
  if (is.null(h_grid) && k > 0) {
    sds <- apply(X, 2, sd, na.rm = TRUE)
    h_grid <- lapply(sds, function(sd) seq(0.1 * sd, 2 * sd, length.out = 5))
  }
  if (is.null(lambda_grid) && p > 0) {
    lambda_grid <- replicate(p, seq(0.01, 0.99, length.out = 5), simplify = FALSE)
  }
  
  # Build full bandwidth grid
  grid <- expand.grid(c(h_grid, lambda_grid))
  colnames(grid) <- c(paste0("h", seq_along(h_grid)), paste0("lambda", seq_along(lambda_grid)))
  n_grid <- nrow(grid)
  n_eval <- nrow(x_grid)
  
  # Define prediction function
  predict_fun <- if (estimator == "NW") nw_circular_mixed else ll_circular_mixed
  
  # Loop over bandwidths
  scores <- numeric(n_grid)
  for (g in seq_len(n_grid)) {
    h <- if (k > 0) as.numeric(grid[g, 1:k]) else NULL
    lambda <- if (p > 0) as.numeric(grid[g, (k + 1):(k + p)]) else NULL
    
    cmise_vals <- numeric(B)
    for (b in seq_len(B)) {
      m_hat <- sapply(1:n_eval, function(i) {
        x0 <- x_grid[i, , drop = TRUE]
        z0 <- if (!is.null(Z)) Z[(i %% nrow(Z)) + 1, , drop = FALSE] else NULL
        
        predict_fun(
          x0 = x0,
          z0 = if (!is.null(z0)) z0 else NULL,
          X = X,
          Z = Z,
          theta = theta,
          h = h,
          lambda = lambda,
          kernel_continuous = kernel_continuous,
          kernel_categorical = kernel_categorical,
          kernel_levels = kernel_levels
        )
      })
      
      cmise_vals[b] <- mean(1 - cos(m_hat - m_true), na.rm = TRUE)
    }
    
    scores[g] <- mean(cmise_vals)
  }
  
  best_idx <- which.min(scores)
  list(
    h_oracle = as.numeric(grid[best_idx, 1:length(h_grid)]),
    lambda_oracle = as.numeric(grid[best_idx, (length(h_grid) + 1):ncol(grid)]),
    cmise_oracle = scores[best_idx]
  )
}
