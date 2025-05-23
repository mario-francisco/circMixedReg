% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_circular_data.R
\name{simulate_circular_data}
\alias{simulate_circular_data}
\title{Simulate Circular Regression Data with Mixed Predictors}
\usage{
simulate_circular_data(
  n,
  mu_fun,
  kappa = 4,
  x_range = c(0, 1),
  z_levels = c("A", "B", "C"),
  seed = NULL
)
}
\arguments{
\item{n}{Integer. Sample size (number of observations).}

\item{mu_fun}{Function of two arguments (x, z) returning the true mean direction (angle in radians).}

\item{kappa}{Numeric. Concentration parameter of the von Mises error distribution (higher values produce less noise).}

\item{x_range}{Numeric vector of length 2. Range (minimum and maximum) for the continuous predictor.}

\item{z_levels}{Character vector. Levels for the categorical predictor.}

\item{seed}{Optional integer. Random seed for reproducibility.}
}
\value{
A data.frame with columns:
\describe{
\item{theta}{Observed circular response (radians, in [0, 2pi)).}
\item{X}{Continuous predictor.}
\item{Z}{Categorical predictor (factor).}
\item{mu}{True mean direction used to generate the response (radians, in [0, 2pi)).}
}
}
\description{
Generates a synthetic dataset for circular regression with one continuous and one categorical predictor.
}
\examples{
# Define a true regression function
mu_fun <- function(x, z) {
  base_angle <- ifelse(z == "A", 0, ifelse(z == "B", pi/2, pi))
  (base_angle + 2 * pi * x) \%\% (2 * pi)
}

# Simulate data
set.seed(123)
sim_data <- simulate_circular_data(n = 100, mu_fun = mu_fun, kappa = 5)

head(sim_data)

}
