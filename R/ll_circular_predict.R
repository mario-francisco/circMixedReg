#' Predict Circular Regression with Local Linear Estimator
#'
#' Predicts values from a local linear circular regression model with mixed predictors
#' at multiple target points using product-kernel smoothing.
#'
#' @param data Data frame containing the original dataset with predictors and response.
#' @param new_data Data frame with the values of the predictors at which to make predictions.
#' @param response_col Name of the column with the circular response (default: \code{"theta"}).
#' @param cont_cols Character vector with the names of the continuous predictors.
#' @param cat_cols Character vector with the names of the categorical predictors.
#' @param h Bandwidth(s) for the continuous variables (scalar or vector).
#' @param lambda Smoothing parameter(s) for the categorical variables (scalar or vector).
#' @param kernel_continuous Kernel function for continuous predictors.
#' @param kernel_categorical Kernel function for categorical predictors.
#' @param kernel_levels Optional list of level vectors (for ordinal or other kernels requiring level info).
#' @param leave_one_out Logical. If TRUE and \code{data == new_data}, applies leave-one-out strategy.
#'
#' @return A vector of predicted circular values (class \code{circular}) at the new points.
#'
#' @importFrom stats runif
#' @importFrom circular circular
#'
#' @examples
#' set.seed(123)
#' n <- 50
#' df <- data.frame(
#'   x = runif(n),
#'   z = sample(c("A", "B", "C"), n, replace = TRUE)
#' )
#' angle <- 2 * pi * df$x + rnorm(n, sd = 0.3)
#' df$theta <- circular::circular(angle, units = "radians", modulo = "2pi")#'
#' new_points <- df[1:5, ]
#' levels_list <- list(z = c("A", "B", "C"))
#'
#' ll_circular_predict(
#'   data = df,
#'   new_data = new_points,
#'   response_col = "theta",
#'   cont_cols = "x",
#'   cat_cols = "z",
#'   h = 0.2,
#'   lambda = 0.3,
#'   kernel_continuous = gaussian_kernel,
#'   kernel_categorical = ordinal_kernel,
#'   kernel_levels = levels_list
#' )
#'
#' @export
ll_circular_predict <- function(data, new_data,
                                response_col = "theta",
                                cont_cols = NULL,
                                cat_cols = NULL,
                                h = 0.5, lambda = 0.5,
                                kernel_continuous = gaussian_kernel,
                                kernel_categorical = aitchison_aitken_kernel,
                                kernel_levels = NULL,
                                leave_one_out = FALSE) {
  
  # --- Validations ---
  if (!response_col %in% names(data)) stop("Response column not found in 'data'.")
  
  if (!is.null(cont_cols) && !all(cont_cols %in% names(data))) {
    stop("Some continuous predictor names not found in 'data'.")
  }
  if (!is.null(cat_cols) && !all(cat_cols %in% names(data))) {
    stop("Some categorical predictor names not found in 'data'.")
  }
  
  theta <- circular(data[[response_col]], units = "radians", modulo = "2pi")
  X <- if (!is.null(cont_cols)) as.matrix(data[cont_cols]) else NULL
  Z <- if (!is.null(cat_cols)) as.data.frame(data[cat_cols]) else NULL
  X_new <- if (!is.null(cont_cols)) as.matrix(new_data[cont_cols]) else NULL
  Z_new <- if (!is.null(cat_cols)) as.data.frame(new_data[cat_cols]) else NULL
  
  if (!is.null(X) && length(h) == 1) h <- rep(h, ncol(X))
  if (!is.null(Z) && length(lambda) == 1) lambda <- rep(lambda, ncol(Z))
  
  if (!is.null(X) && length(h) != ncol(X)) {
    stop("Length of 'h' must match number of continuous predictors.")
  }
  if (!is.null(Z) && length(lambda) != ncol(Z)) {
    stop("Length of 'lambda' must match number of categorical predictors.")
  }
  
  if (!is.null(Z) && is.null(kernel_levels)) {
    kernel_levels <- lapply(Z, function(col) sort(unique(col)))
  }
  
  preds <- numeric(nrow(new_data))
  
  for (i in seq_len(nrow(new_data))) {
    if (leave_one_out && identical(data, new_data)) {
      idx <- setdiff(seq_len(nrow(data)), i)
      preds[i] <- ll_circular_mixed(
        x0 = if (!is.null(X_new)) X_new[i, , drop = TRUE] else numeric(0),
        z0 = if (!is.null(Z_new)) Z_new[i, , drop = TRUE] else character(0),
        X = if (!is.null(X)) X[idx, , drop = FALSE] else NULL,
        Z = if (!is.null(Z)) Z[idx, , drop = FALSE] else NULL,
        theta = theta[idx],
        h = h,
        lambda = lambda,
        kernel_continuous = kernel_continuous,
        kernel_categorical = kernel_categorical,
        kernel_levels = kernel_levels
      )
    } else {
      preds[i] <- ll_circular_mixed(
        x0 = if (!is.null(X_new)) X_new[i, , drop = TRUE] else numeric(0),
        z0 = if (!is.null(Z_new)) Z_new[i, , drop = TRUE] else character(0),
        X = X,
        Z = Z,
        theta = theta,
        h = h,
        lambda = lambda,
        kernel_continuous = kernel_continuous,
        kernel_categorical = kernel_categorical,
        kernel_levels = kernel_levels
      )
    }
  }
  
  return(circular(preds, units = "radians", modulo = "2pi"))
}
