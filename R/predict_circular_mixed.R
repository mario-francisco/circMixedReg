#' Predict Circular Regression with Mixed Predictors (NW or LL)
#'
#' General wrapper to predict values from a circular regression model with mixed predictors
#' using either Nadaraya-Watson or local linear kernel smoothing.
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
#' @param estimator Character string: either \code{"NW"} (default) or \code{"LL"}.
#' @param leave_one_out Logical. If TRUE and \code{data == new_data}, applies leave-one-out strategy.
#'
#' @return A vector of predicted circular values (class \code{circular}) at the new points.
#'
#' @importFrom circular circular
#'
#' @examples
#' set.seed(123)
#' n <- 100
#' x <- runif(n)
#' z <- sample(c("A", "B", "C"), n, replace = TRUE)
#' mu <- function(x, z) ifelse(z == "A", sin(2 * pi * x),
#'                        ifelse(z == "B", cos(2 * pi * x),
#'                        2 * pi * x %% (2 * pi)))
#' theta <- (mu(x, z) + circular::rvonmises(n, mu = circular::circular(0), kappa = 3)) %% (2 * pi)
#' data <- data.frame(x = x, z = z, theta = theta)
#'
#' preds_nw <- predict_circular_mixed(
#'   data = data, new_data = data,
#'   cont_cols = "x", cat_cols = "z",
#'   h = 0.3, lambda = 0.4,
#'   estimator = "NW"
#' )
#'
#' preds_ll <- predict_circular_mixed(
#'   data = data, new_data = data,
#'   cont_cols = "x", cat_cols = "z",
#'   h = 0.3, lambda = 0.4,
#'   estimator = "LL"
#' )
#'
#' mean(1 - cos(theta - preds_nw))  # CMASE NW
#' mean(1 - cos(theta - preds_ll))  # CMASE LL
#'
#' @export
predict_circular_mixed <- function(data, new_data,
                                   response_col = "theta",
                                   cont_cols = NULL,
                                   cat_cols = NULL,
                                   h = 0.5, lambda = 0.5,
                                   kernel_continuous = gaussian_kernel,
                                   kernel_categorical = aitchison_aitken_kernel,
                                   kernel_levels = NULL,
                                   estimator = "NW",
                                   leave_one_out = FALSE) {
  
  estimator <- match.arg(estimator, choices = c("NW", "LL"))
  
  if (estimator == "NW") {
    return(nw_circular_predict(
      data = data,
      new_data = new_data,
      response_col = response_col,
      cont_cols = cont_cols,
      cat_cols = cat_cols,
      h = h,
      lambda = lambda,
      kernel_continuous = kernel_continuous,
      kernel_categorical = kernel_categorical,
      kernel_levels = kernel_levels,
      leave_one_out = leave_one_out
    ))
  } else {
    return(ll_circular_predict(
      data = data,
      new_data = new_data,
      response_col = response_col,
      cont_cols = cont_cols,
      cat_cols = cat_cols,
      h = h,
      lambda = lambda,
      kernel_continuous = kernel_continuous,
      kernel_categorical = kernel_categorical,
      kernel_levels = kernel_levels,
      leave_one_out = leave_one_out
    ))
  }
}
