#' Compute Residuals for Circular Regression (NW or LL)
#'
#' Computes circular residuals between observed values and predicted values
#' from a nonparametric regression model with mixed-type predictors (continuous + categorical),
#' using either Nadaraya-Watson or local linear kernel smoothing.
#'
#' @param data Data frame containing the sample, including the response and predictors.
#' @param response_col Name of the column with the circular response (in radians). Default is `"theta"`.
#' @param cont_cols Character vector with the names of the continuous predictors.
#' @param cat_cols Character vector with the names of the categorical predictors.
#' @param h Vector of bandwidths for continuous predictors (scalar or vector).
#' @param lambda Vector of smoothing parameters for categorical predictors (scalar or vector).
#' @param kernel_continuous Kernel function for continuous predictors.
#' @param kernel_categorical Kernel function for categorical predictors.
#' @param kernel_levels Optional list of level vectors for each categorical variable (used by ordinal kernels).
#' @param estimator Character string: either `"NW"` (default) or `"LL"`.
#'
#' @return A numeric vector of residuals (in radians), normalized to the interval (-pi, pi).
#' 
#' @importFrom stats residuals runif
#' @importFrom circular circular rvonmises
#'
#' @examples
#' # Simulated example
#' set.seed(123)
#' n <- 50
#' x <- runif(n)
#' z <- sample(c("A", "B", "C"), n, replace = TRUE)
#' theta <- (pi * x + circular::rvonmises(n, mu = circular::circular(0), kappa = 3)) %% (2 * pi)
#' df <- data.frame(x = x, z = z, theta = theta)
#'
#' # Compute NW residuals
#' res_nw <- compute_residuals(df, cont_cols = "x", cat_cols = "z",
#'                              h = 0.2, lambda = 0.5,
#'                              kernel_continuous = gaussian_kernel,
#'                              kernel_categorical = aitchison_aitken_kernel,
#'                              estimator = "NW")
#'
#' # Compute LL residuals
#' res_ll <- compute_residuals(df, cont_cols = "x", cat_cols = "z",
#'                              h = 0.2, lambda = 0.5,
#'                              kernel_continuous = gaussian_kernel,
#'                              kernel_categorical = aitchison_aitken_kernel,
#'                              estimator = "LL")
#'
#' summary(res_nw)
#' summary(res_ll)
#'
#' @export
compute_residuals <- function(data,
                              response_col = "theta",
                              cont_cols = NULL,
                              cat_cols = NULL,
                              h = 0.5,
                              lambda = 0.5,
                              kernel_continuous = gaussian_kernel,
                              kernel_categorical = aitchison_aitken_kernel,
                              kernel_levels = NULL,
                              estimator = "NW") {
  
  theta <- circular(data[[response_col]], units = "radians", modulo = "2pi")
  
  preds <- predict_circular_mixed(
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
    estimator = estimator,
    leave_one_out = FALSE
  )
  
  resids <- (theta - preds + pi) %% (2 * pi) - pi
  return(resids)
}
