#' Bootstrap Bandwidth Selector for Circular Regression (Generalized)
#'
#' Selects optimal bandwidths for nonparametric circular regression with mixed-type predictors,
#' by minimizing a bootstrap approximation of the Circular Mean Integrated Squared Error (CMISE).
#'
#' Supports both Nadaraya-Watson (NW) and local linear (LL) estimators.
#'
#' @param data Data frame containing the dataset.
#' @param response_col Name of the column with the circular response (in radians).
#' @param cont_cols Character vector of column names for continuous predictors.
#' @param cat_cols Character vector of column names for categorical predictors.
#' @param h_grid Optional list of numeric vectors for candidate bandwidths of continuous predictors.
#' @param lambda_grid Optional list of numeric vectors for candidate smoothing parameters for categorical predictors.
#' @param h0,lambda0 Pilot bandwidths for residual computation.
#' @param h1,lambda1 Pilot bandwidths for bootstrap pseudo-responses.
#' @param B Number of bootstrap replicates. Default is 100.
#' @param eps_bootstrap Optional matrix of bootstrap errors (n × B) for reproducibility. If NULL (for standard use), they are generated internally.
#' @param kernel_continuous Kernel function for continuous predictors.
#' @param kernel_categorical Kernel function for categorical predictors.
#' @param kernel_levels Optional list of level vectors for each categorical predictor (needed for ordinal kernels).
#' @param estimator Either `"NW"` (default) or `"LL"`, indicating the type of regression estimator to use.
#'
#' @return A list with optimal `h_boot`, `lambda_boot`, and the minimum bootstrap CMISE.
#'
#' @note
#' To ensure exact reproducibility across runs (e.g., when comparing to a parallel version), 
#' you can precompute the bootstrap residuals externally with a fixed seed and pass them 
#' via the `eps_bootstrap` argument as a matrix of size `n x B`. Otherwise, the function 
#' will sample residuals internally using `sample()`.
#'
#' @importFrom stats runif sd
#' @importFrom circular circular rvonmises
#'
#' @examples
#' set.seed(42)
#' n <- 20
#' x <- runif(n)
#' z <- sample(c("A", "B", "C"), n, replace = TRUE)
#' mu <- function(x, z) {
#'   if (z == "A") return(pi * x)
#'   if (z == "B") return(pi / 2 * x + pi / 4)
#'   return(pi * (1 - x))
#' }
#' theta <- sapply(1:n, function(i) {
#'   (mu(x[i], z[i]) + circular::rvonmises(1, mu = circular::circular(0), kappa = 5)) %% (2 * pi)
#' })
#' df <- data.frame(x = x, z = z, theta = theta)
#'
#' result <- bootstrap_bandwidth_selector(
#'   data = df,
#'   response_col = "theta",
#'   cont_cols = "x",
#'   cat_cols = "z",
#'   h0 = 0.2, lambda0 = 0.4,
#'   h1 = 0.3, lambda1 = 0.4,
#'   estimator = "NW",
#'   B = 5,  # menor número de réplicas bootstrap
#'   kernel_continuous = gaussian_kernel,
#'   kernel_categorical = aitchison_aitken_kernel,
#'   kernel_levels = list(z = c("A", "B", "C"))
#' )
#' print(result)
#'
#' @export
bootstrap_bandwidth_selector <- function(data,
                                         response_col = "theta",
                                         cont_cols = NULL,
                                         cat_cols = NULL,
                                         h_grid = NULL,
                                         lambda_grid = NULL,
                                         h0, lambda0,
                                         h1, lambda1,
                                         B = 100,
                                         eps_bootstrap = NULL,
                                         kernel_continuous = gaussian_kernel,
                                         kernel_categorical = aitchison_aitken_kernel,
                                         kernel_levels = NULL,
                                         estimator = "NW") {
  if (!requireNamespace("circular", quietly = TRUE)) {
    stop("The 'circular' package is required. Please install it.")
  }
  
  theta <- circular::circular(data[[response_col]], units = "radians", modulo = "2pi")
  X <- if (!is.null(cont_cols)) as.matrix(data[cont_cols]) else NULL
  Z <- if (!is.null(cat_cols)) data[cat_cols] else NULL
  n <- length(theta)
  
  estimator <- match.arg(estimator, choices = c("NW", "LL"))
  predict_fun <- if (estimator == "NW") nw_circular_mixed else ll_circular_mixed
  
  # Paso 1: estimación inicial
  m0 <- sapply(1:n, function(i) predict_fun(
    x0 = if (!is.null(X)) X[i, ] else NULL,
    z0 = if (!is.null(Z)) Z[i, ] else NULL,
    X = X, Z = Z, theta = theta,
    h = h0, lambda = lambda0,
    kernel_continuous = kernel_continuous,
    kernel_categorical = kernel_categorical,
    kernel_levels = kernel_levels
  ))
  
  eps_hat <- (theta - m0 + pi) %% (2 * pi) - pi
  mean_sin <- mean(sin(eps_hat))
  mean_cos <- mean(cos(eps_hat))
  mean_dir <- atan2(mean_sin, mean_cos)
  eps_tilde <- (eps_hat - mean_dir + pi) %% (2 * pi) - pi
  
  # Paso 2: construir la rejilla
  if (is.null(h_grid) && !is.null(X)) {
    sds <- apply(X, 2, sd, na.rm = TRUE)
    h_grid <- lapply(sds, function(sd) seq(0.1 * sd, 2 * sd, length.out = 5))
  }
  if (is.null(lambda_grid) && !is.null(Z)) {
    lambda_grid <- replicate(ncol(Z), seq(0.01, 0.99, length.out = 5), simplify = FALSE)
  }
  
  grid <- expand.grid(c(h_grid, lambda_grid))
  colnames(grid) <- c(paste0("h", seq_along(h_grid)), paste0("lambda", seq_along(lambda_grid)))
  
  # Paso 3: estimación con (h1, lambda1)
  m1 <- sapply(1:n, function(i) predict_fun(
    x0 = if (!is.null(X)) X[i, ] else NULL,
    z0 = if (!is.null(Z)) Z[i, ] else NULL,
    X = X, Z = Z, theta = theta,
    h = h1, lambda = lambda1,
    kernel_continuous = kernel_continuous,
    kernel_categorical = kernel_categorical,
    kernel_levels = kernel_levels
  ))
  
  # Paso 4: bootstrap
  scores <- numeric(nrow(grid))
  
  # Generar perturbaciones externas si no se proporcionaron
  if (is.null(eps_bootstrap)) {
    eps_bootstrap <- replicate(B, sample(eps_tilde, n, replace = TRUE))
  }
  
  for (g in seq_len(nrow(grid))) {
    h <- as.numeric(grid[g, 1:length(h_grid)])
    lambda <- as.numeric(grid[g, (length(h_grid) + 1):ncol(grid)])
    message("Evaluating grid point ", g, " of ", nrow(grid), " ...")
    
    cmise_vals <- numeric(B)
    for (b in seq_len(B)) {
      eps_b <- eps_bootstrap[, b]
      theta_b <- (m1 + eps_b + pi) %% (2 * pi)
      
      m_b <- sapply(1:n, function(i) predict_fun(
        x0 = if (!is.null(X)) X[i, ] else NULL,
        z0 = if (!is.null(Z)) Z[i, ] else NULL,
        X = X, Z = Z, theta = theta_b,
        h = h, lambda = lambda,
        kernel_continuous = kernel_continuous,
        kernel_categorical = kernel_categorical,
        kernel_levels = kernel_levels
      ))
      
      cmise_vals[b] <- mean(1 - cos(m_b - m1), na.rm = TRUE)
    }
    
    scores[g] <- mean(cmise_vals)
  }
  
  best_idx <- which.min(scores)
  list(
    h_boot = as.numeric(grid[best_idx, 1:length(h_grid)]),
    lambda_boot = as.numeric(grid[best_idx, (length(h_grid) + 1):ncol(grid)]),
    cmise = scores[best_idx]
  )
}
