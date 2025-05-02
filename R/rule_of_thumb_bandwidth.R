#' Rule of Thumb Bandwidth Selector for Circular Regression Estimation
#'
#' Provides heuristic values for bandwidths in circular regression models
#' with continuous and categorical predictors, adapted to either
#' Nadaraya--Watson (NW) or local linear (LL) estimators.
#'
#' The rule applies classic scaling laws based on asymptotic MSE minimization.
#' Different constants can be used depending on the estimator.
#'
#' @param data Data frame containing the sample.
#' @param cont_cols Character vector of names of continuous predictors.
#' @param cat_cols Character vector of names of categorical predictors.
#' @param estimator Character string: either "NW" (default) or "LL".
#' @param ch_NW,clambda_NW Constants for NW selector (defaults: 1.06 and 1.0).
#' @param ch_LL,clambda_LL Constants for LL selector (defaults: 1.20 and 1.0).
#' @param exponent_cont Exponent for sample size in the rule for continuous predictors (default = 1/5).
#' @param exponent_cat Exponent for sample size in the rule for categorical predictors (default = 1/5).
#' @param alpha Exponent for the number of levels of each categorical predictor (default = 1).
#'
#' @return A list with numeric vectors \code{h} (for continuous predictors) and \code{lambda} (for categorical ones).
#' 
#' @importFrom stats runif sd
#' @importFrom circular circular 
#'
#' @examples
#' data <- data.frame(x = runif(100), z = sample(letters[1:3], 100, replace = TRUE))
#' rule_of_thumb_bandwidth(data, cont_cols = "x", cat_cols = "z", estimator = "LL")
#'
#' @export
rule_of_thumb_bandwidth <- function(data,
                                    cont_cols = NULL,
                                    cat_cols = NULL,
                                    estimator = "NW",
                                    ch_NW = 1.06,
                                    clambda_NW = 1.0,
                                    ch_LL = 1.20,
                                    clambda_LL = 1.0,
                                    exponent_cont = 1/5,
                                    exponent_cat = 1/5,
                                    alpha = 1) {
  
  estimator <- match.arg(estimator, choices = c("NW", "LL"))
  n <- nrow(data)
  
  # Set constants based on estimator
  if (estimator == "NW") {
    ch <- ch_NW
    clambda <- clambda_NW
  } else {
    ch <- ch_LL
    clambda <- clambda_LL
  }
  
  # Bandwidths for continuous predictors
  h <- NULL
  if (!is.null(cont_cols)) {
    h <- sapply(cont_cols, function(col) {
      sx <- stats::sd(data[[col]], na.rm = TRUE)
      ch * sx * n^(-exponent_cont)
    })
    names(h) <- cont_cols
  }
  
  # Smoothing parameters for categorical predictors
  lambda <- NULL
  if (!is.null(cat_cols)) {
    lambda <- sapply(cat_cols, function(col) {
      L <- length(unique(data[[col]]))
      if (L < 2) stop(sprintf("Categorical predictor '%s' must have at least 2 levels.", col))  # safeguard
      clambda / (L^alpha) * n^(-exponent_cat)
    })
    names(lambda) <- cat_cols
  }
  
  return(list(h = h, lambda = lambda))
}
