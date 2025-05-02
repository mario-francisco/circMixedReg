#' Bootstrap Bandwidth Selector (Parallel) for Circular Regression
#'
#' Selects optimal bandwidths for nonparametric circular regression with mixed-type predictors,
#' by minimizing a bootstrap approximation of the Circular Mean Integrated Squared Error (CMISE).
#' This version uses parallel evaluation over the bandwidth grid using `future_sapply()`, 
#' with proper handling of random number generation to ensure reproducibility.
#'
#' @param data A data frame containing the dataset.
#' @param response_col Name of the column with the circular response (in radians).
#' @param cont_cols Character vector with the names of the continuous predictor columns.
#' @param cat_cols Character vector with the names of the categorical predictor columns.
#' @param h_grid List of candidate bandwidth vectors for the continuous predictors. If NULL, a default grid is generated.
#' @param lambda_grid List of candidate lambda vectors for the categorical predictors. If NULL, a default grid is generated.
#' @param h0,lambda0 Pilot bandwidths used to estimate residuals.
#' @param h1,lambda1 Pilot bandwidths used to generate pseudo-responses.
#' @param B Number of bootstrap replicates to approximate the CMISE.
#' @param eps_bootstrap Optional matrix of bootstrap errors (n x B) for reproducibility. If NULL (for standard use), they are generated internally.
#' @param kernel_continuous Kernel function for continuous predictors.
#' @param kernel_categorical Kernel function for categorical predictors.
#' @param kernel_levels Optional list of levels for each categorical variable (needed if `kernel_categorical` is ordinal).
#' @param estimator Either "NW" (default) or "LL" to choose the regression method.
#' @param parallel Logical. Whether to use parallel evaluation across the grid (default TRUE).
#'
#' @return A list with the selected bandwidths \code{h_boot}, \code{lambda_boot}, and the estimated CMISE.
#'
#' @note 
#' To ensure exact reproducibility across parallel and sequential versions, you can externally 
#' generate the same bootstrap residual matrix (n x B) and pass it to both functions via the 
#' `eps_bootstrap` argument. Otherwise, internal sampling may differ.
#'
#' This function uses parallel computing via the \pkg{future} and \pkg{future.apply} packages.
#' To enable parallel execution, configure the parallel plan at the beginning of the session using:
#' \code{future::plan(future::multisession, workers = N)}, where \code{N} is the number of CPU cores.
#'
#' @importFrom stats residuals runif sd
#' @importFrom circular circular rvonmises
#' @importFrom future plan
#' @importFrom future.apply future_sapply
#'
#' @export
bootstrap_bandwidth_selector_parallel <- function(data,
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
                                                  estimator = "NW",
                                                  parallel = TRUE) {
  if (!requireNamespace("circular", quietly = TRUE)) stop("Package 'circular' is required.")
  estimator <- match.arg(estimator, choices = c("NW", "LL"))
  predict_fun <- if (estimator == "NW") nw_circular_mixed else ll_circular_mixed
  
  if (parallel && (!requireNamespace("future.apply", quietly = TRUE))) {
    warning("Parallel disabled: 'future.apply' not available.")
    parallel <- FALSE
  }
  
  theta <- circular(data[[response_col]], units = "radians", modulo = "2pi")
  X <- if (!is.null(cont_cols)) as.matrix(data[cont_cols]) else NULL
  Z <- if (!is.null(cat_cols)) data[cat_cols] else NULL
  n <- length(theta)
  k <- if (!is.null(X)) ncol(X) else 0
  p <- if (!is.null(Z)) ncol(Z) else 0
  
  if (is.null(h_grid) && k > 0) {
    sds <- apply(X, 2, sd, na.rm = TRUE)
    h_grid <- lapply(sds, function(sd) seq(0.1 * sd, 2 * sd, length.out = 5))
  }
  if (is.null(lambda_grid) && !is.null(Z)) {
    lambda_grid <- replicate(ncol(Z), seq(0.01, 1, length.out = 5), simplify = FALSE)
  }
  grid <- expand.grid(c(h_grid, lambda_grid))
  
  m0 <- predict_circular_mixed(
    data = data, new_data = data,
    response_col = response_col,
    cont_cols = cont_cols, cat_cols = cat_cols,
    h = h0, lambda = lambda0,
    kernel_continuous = kernel_continuous, kernel_categorical = kernel_categorical,
    kernel_levels = kernel_levels,
    estimator = estimator,
    leave_one_out = TRUE
  )
  eps_hat <- (theta - m0 + pi) %% (2 * pi) - pi
  eps_bar <- atan2(mean(sin(eps_hat)), mean(cos(eps_hat)))
  eps_tilde <- (eps_hat - eps_bar + pi) %% (2 * pi) - pi
  
  m1 <- predict_circular_mixed(
    data = data, new_data = data,
    response_col = response_col,
    cont_cols = cont_cols, cat_cols = cat_cols,
    h = h1, lambda = lambda1,
    kernel_continuous = kernel_continuous, kernel_categorical = kernel_categorical,
    kernel_levels = kernel_levels,
    estimator = estimator,
    leave_one_out = FALSE
  )
  
  if (is.null(eps_bootstrap)) {
    eps_bootstrap <- replicate(B, sample(eps_tilde, n, replace = TRUE))
  }
  
  evaluate_grid <- function(g) {
    h <- if (k > 0) as.numeric(grid[g, 1:k]) else NULL
    lambda <- if (p > 0) as.numeric(grid[g, (k + 1):(k + p)]) else NULL
    
    cmise_vals <- numeric(B)
    for (b in seq_len(B)) {
      theta_b <- (m1 + eps_bootstrap[, b] + pi) %% (2 * pi)
      
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
    mean(cmise_vals)
  }
  
  if (parallel) {
    old_plan <- future::plan()
    future::plan(future::multisession)
    on.exit(future::plan(old_plan), add = TRUE)
    scores <- future.apply::future_sapply(seq_len(nrow(grid)), evaluate_grid, future.seed = TRUE)
  } else {
    scores <- sapply(seq_len(nrow(grid)), evaluate_grid)
  }
  
  best <- which.min(scores)
  list(
    h_boot = if (k > 0) as.numeric(grid[best, 1:k]) else NULL,
    lambda_boot = if (p > 0) as.numeric(grid[best, (k + 1):(k + p)]) else NULL,
    cmise = scores[best]
  )
}


