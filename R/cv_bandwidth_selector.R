#' Cross-Validation Bandwidth Selector for Circular Regression (NW or LL)
#'
#' Selects optimal bandwidths for a circular regression model with mixed-type predictors
#' using leave-one-out cross-validation (LOOCV), minimizing the circular prediction error.
#'
#' Compatible with both Nadaraya-Watson ("NW") and local linear ("LL") estimators.
#'
#' @param data A data frame with the sample, including the circular response and predictors.
#' @param response_col Name of the circular response variable (in radians). Default: "theta".
#' @param cont_cols Character vector with names of continuous predictors (optional).
#' @param cat_cols Character vector with names of categorical predictors (optional).
#' @param kernel_continuous Kernel function for continuous predictors (default: `gaussian_kernel`).
#' @param kernel_categorical Kernel function for categorical predictors (default: `aitchison_aitken_kernel`).
#' @param h_grid Optional list of candidate bandwidth vectors for continuous variables. If NULL, defaults are created.
#' @param lambda_grid Optional list of candidate lambda vectors for categorical variables. If NULL, defaults are created.
#' @param estimator Either `"NW"` (default) or `"LL"` indicating the regression estimator to use.
#' @param kernel_levels Optional list of level vectors for each categorical predictor (required for ordinal kernels).
#' @param verbose Logical; if TRUE, prints progress messages (default: TRUE).
#'
#' @return A list with:
#'   \item{h_cv}{Selected bandwidths for continuous predictors.}
#'   \item{lambda_cv}{Selected lambdas for categorical predictors.}
#'   \item{cv_error}{Minimum cross-validation error.}
#'   \item{cv_grid}{Data frame of all grid combinations.}
#'   \item{error_grid}{Vector of CV errors per grid point.}
#'
#' @importFrom circular circular
#'
#' @examples
#' set.seed(123)
#' n <- 50
#' x <- runif(n)
#' z <- sample(c("A", "B", "C"), n, replace = TRUE)
#' mu <- function(x, z) {
#'   if (z == "A") return(pi * x)
#'   if (z == "B") return(pi / 2 * x + pi / 4)
#'   return(pi * (1 - x))
#' }
#' theta <- sapply(1:n, function(i) {
#'   (mu(x[i], z[i]) + circular::rvonmises(1, mu = circular::circular(0), kappa = 4)) %% (2 * pi)
#' })
#' data <- data.frame(x = x, z = z, theta = theta)
#'
#' # Run CV selector for NW
#' result_nw <- cv_bandwidth_selector(
#'   data = data,
#'   response_col = "theta",
#'   cont_cols = "x",
#'   cat_cols = "z",
#'   estimator = "NW",
#'   kernel_levels = list(z = c("A", "B", "C")),
#'   verbose = FALSE
#' )
#'
#' # Run CV selector for LL
#' result_ll <- cv_bandwidth_selector(
#'   data = data,
#'   response_col = "theta",
#'   cont_cols = "x",
#'   cat_cols = "z",
#'   estimator = "LL",
#'   kernel_levels = list(z = c("A", "B", "C")),
#'   verbose = FALSE
#' )
#'
#' print(result_nw$h_cv)
#' print(result_ll$h_cv)
#'
#' @export
cv_bandwidth_selector <- function(data,
                                  response_col = "theta",
                                  cont_cols = NULL,
                                  cat_cols = NULL,
                                  kernel_continuous = gaussian_kernel,
                                  kernel_categorical = aitchison_aitken_kernel,
                                  h_grid = NULL,
                                  lambda_grid = NULL,
                                  estimator = "NW",
                                  kernel_levels = NULL,
                                  verbose = TRUE) {
  estimator <- match.arg(estimator, choices = c("NW", "LL"))
  predict_fun <- if (estimator == "NW") nw_circular_predict else ll_circular_predict
  
  theta <- circular(data[[response_col]], units = "radians", modulo = "2pi")
  X <- if (!is.null(cont_cols)) as.matrix(data[cont_cols]) else NULL
  Z <- if (!is.null(cat_cols)) data[cat_cols] else NULL
  
  k <- if (!is.null(X)) ncol(X) else 0
  p <- if (!is.null(Z)) ncol(Z) else 0
  
  # Default grid generation (coherent with bootstrap)
  if (is.null(h_grid) && k > 0) {
    sds <- apply(X, 2, sd, na.rm = TRUE)
    h_grid <- lapply(sds, function(sd) seq(0.1 * sd, 2 * sd, length.out = 5))
  }
  if (is.null(lambda_grid) && p > 0) {
    lambda_grid <- replicate(p, seq(0.01, 0.99, length.out = 5), simplify = FALSE)
  }
  
  grid_list <- c(h_grid, lambda_grid)
  grid_df <- expand.grid(grid_list)
  n_grid <- nrow(grid_df)
  
  best_score <- Inf
  best_h <- NULL
  best_lambda <- NULL
  cv_errors <- numeric(n_grid)
  
  for (g in seq_len(n_grid)) {
    h <- if (k > 0) as.numeric(grid_df[g, 1:k]) else NULL
    lambda <- if (p > 0) as.numeric(grid_df[g, (k + 1):(k + p)]) else NULL
    
    preds <- predict_fun(
      data = data,
      new_data = data,
      response_col = response_col,
      cont_cols = cont_cols,
      cat_cols = cat_cols,
      h = h,
      lambda = lambda,
      kernel_continuous = kernel_continuous,
      kernel_categorical = kernel_categorical,
      kernel_levels = kernel_levels,
      leave_one_out = TRUE
    )
    
    valid <- !is.na(preds)
    error <- mean(1 - cos(theta[valid] - preds[valid]))
    cv_errors[g] <- error
    
    if (verbose) {
      cat(sprintf("Grid %3d/%d -> CV Error = %.5f\n", g, n_grid, error))
    }
    
    if (error < best_score) {
      best_score <- error
      best_h <- h
      best_lambda <- lambda
    }
  }
  
  list(
    h_cv = best_h,
    lambda_cv = best_lambda,
    cv_error = best_score,
    cv_grid = grid_df,
    error_grid = cv_errors
  )
}
