#' Simulate Circular Regression Data with Mixed Predictors (1 or 2 continuous + 1 categorical)
#'
#' Generates a synthetic dataset for circular regression with one or two continuous predictors and one categorical predictor.
#'
#' @param n Integer. Sample size (number of observations).
#' @param mu_fun A function of x, z, and optionally w, returning the true mean direction (angle in radians).
#' @param kappa Numeric. Concentration parameter of the von Mises error distribution.
#' @param x_range Numeric vector of length 2. Range for the first continuous predictor.
#' @param w_range Optional numeric vector of length 2. Range for the second continuous predictor. If NULL, only one continuous predictor is used.
#' @param z_levels Character vector. Levels for the categorical predictor.
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{theta}{Observed circular response (radians, in \code{[0, 2pi)}).}
#'   \item{X}{First continuous predictor.}
#'   \item{Z}{Categorical predictor (factor).}
#'   \item{W}{Second continuous predictor (if applicable).}
#'   \item{mu}{True mean direction (radians, in \code{[0, 2pi)}).}
#' }
#' 
#' @importFrom stats runif
#' @importFrom circular circular rvonmises 

#'
#' @examples
#' # Example with one continuous predictor (X)
#' mu_fun1 <- function(x, z) {
#'   offset <- switch(z, A = 0, B = pi/2, C = pi)
#'   (offset + 2 * pi * x) %% (2 * pi)
#' }
#' sim1 <- simulate_circular_data_gen(n = 100, mu_fun = mu_fun1, kappa = 5)
#'
#' # Example with two continuous predictors (X and W)
#' mu_fun2 <- function(x, z, w) {
#'   base <- switch(z, A = 0, B = pi/2, C = pi)
#'   (base + 2 * pi * x + pi * w) %% (2 * pi)
#' }
#' sim2 <- simulate_circular_data_gen(n = 100, mu_fun = mu_fun2,
#'                                kappa = 5, w_range = c(0, 1))
#'
#' @export
simulate_circular_data_gen <- function(n,
                                   mu_fun,
                                   kappa = 4,
                                   x_range = c(0, 1),
                                   w_range = NULL,
                                   z_levels = c("A", "B", "C"),
                                   seed = NULL) {
  # Basic checks
  stopifnot(is.numeric(n), n > 0,
            is.function(mu_fun),
            is.numeric(kappa), kappa >= 0,
            is.numeric(x_range), length(x_range) == 2,
            is.character(z_levels), length(z_levels) > 0)
  if (!is.null(w_range)) {
    stopifnot(is.numeric(w_range), length(w_range) == 2)
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # Simulate predictors
  X <- runif(n, min = x_range[1], max = x_range[2])
  Z <- sample(z_levels, size = n, replace = TRUE)
  
  if (is.null(w_range)) {
    # Case: one continuous predictor
    mu_vals <- as.numeric(mapply(mu_fun, X, Z))
    W <- NULL
  } else {
    # Case: two continuous predictors
    W <- runif(n, min = w_range[1], max = w_range[2])
    mu_vals <- as.numeric(mapply(mu_fun, X, Z, W))
  }
  
  mu_vals <- mu_vals %% (2 * pi)  # wrap into [0, 2pi)
  
  # Add circular noise
  eps <- rvonmises(n, mu = circular::circular(0), kappa = kappa)
  theta <- (mu_vals + eps) %% (2 * pi)
  
  # Construct output
  df <- data.frame(
    theta = theta,
    X = X,
    Z = factor(Z, levels = z_levels),
    mu = mu_vals
  )
  if (!is.null(W)) {
    df$W <- W
    df <- df[, c("theta", "X", "W", "Z", "mu")]
  }
  
  return(df)
}
