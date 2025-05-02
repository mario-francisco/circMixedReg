#' Simulate Circular Regression Data with Mixed Predictors
#'
#' Generates a synthetic dataset for circular regression with one continuous and one categorical predictor.
#'
#' @param n Integer. Sample size (number of observations).
#' @param mu_fun Function of two arguments (x, z) returning the true mean direction (angle in radians).
#' @param kappa Numeric. Concentration parameter of the von Mises error distribution (higher values produce less noise).
#' @param x_range Numeric vector of length 2. Range (minimum and maximum) for the continuous predictor.
#' @param z_levels Character vector. Levels for the categorical predictor.
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{theta}{Observed circular response (radians, in [0, 2pi)).}
#'   \item{X}{Continuous predictor.}
#'   \item{Z}{Categorical predictor (factor).}
#'   \item{mu}{True mean direction used to generate the response (radians, in [0, 2pi)).}
#' }
#' 
#' @importFrom stats runif
#' @importFrom circular circular rvonmises
#' 
#' @examples
#' # Define a true regression function
#' mu_fun <- function(x, z) {
#'   base_angle <- ifelse(z == "A", 0, ifelse(z == "B", pi/2, pi))
#'   (base_angle + 2 * pi * x) %% (2 * pi)
#' }
#'
#' # Simulate data
#' set.seed(123)
#' sim_data <- simulate_circular_data(n = 100, mu_fun = mu_fun, kappa = 5)
#'
#' head(sim_data)
#'
#' @export
simulate_circular_data <- function(n,
                                   mu_fun,
                                   kappa = 4,
                                   x_range = c(0, 1),
                                   z_levels = c("A", "B", "C"),
                                   seed = NULL) {
  # Basic checks
  stopifnot(is.numeric(n), n > 0,
            is.function(mu_fun),
            is.numeric(kappa), kappa >= 0,
            is.numeric(x_range), length(x_range) == 2,
            is.character(z_levels), length(z_levels) > 0)
  
  if (!is.null(seed)) set.seed(seed)
  
  # Simulate predictors
  X <- runif(n, min = x_range[1], max = x_range[2])
  Z <- sample(z_levels, size = n, replace = TRUE)
  
  # True regression function
  mu_vals <- as.numeric(mapply(mu_fun, X, Z))
  mu_vals <- mu_vals %% (2 * pi)  # wrap into [0, 2pi)
  
  # Add circular noise
  eps <- circular::rvonmises(n, mu = circular::circular(0), kappa = kappa)
  theta <- (mu_vals + eps) %% (2 * pi)
  
  # Return data.frame
  data.frame(
    theta = theta,
    X = X,
    Z = factor(Z, levels = z_levels),
    mu = mu_vals
  )
}
