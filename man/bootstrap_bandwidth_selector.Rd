% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_bandwidth_selector.R
\name{bootstrap_bandwidth_selector}
\alias{bootstrap_bandwidth_selector}
\title{Bootstrap Bandwidth Selector for Circular Regression (Generalized)}
\usage{
bootstrap_bandwidth_selector(
  data,
  response_col = "theta",
  cont_cols = NULL,
  cat_cols = NULL,
  h_grid = NULL,
  lambda_grid = NULL,
  h0,
  lambda0,
  h1,
  lambda1,
  B = 100,
  eps_bootstrap = NULL,
  kernel_continuous = gaussian_kernel,
  kernel_categorical = aitchison_aitken_kernel,
  kernel_levels = NULL,
  estimator = "NW"
)
}
\arguments{
\item{data}{Data frame containing the dataset.}

\item{response_col}{Name of the column with the circular response (in radians).}

\item{cont_cols}{Character vector of column names for continuous predictors.}

\item{cat_cols}{Character vector of column names for categorical predictors.}

\item{h_grid}{Optional list of numeric vectors for candidate bandwidths of continuous predictors.}

\item{lambda_grid}{Optional list of numeric vectors for candidate smoothing parameters for categorical predictors.}

\item{h0, lambda0}{Pilot bandwidths for residual computation.}

\item{h1, lambda1}{Pilot bandwidths for bootstrap pseudo-responses.}

\item{B}{Number of bootstrap replicates. Default is 100.}

\item{eps_bootstrap}{Optional matrix of bootstrap errors (n × B) for reproducibility. If NULL (for standard use), they are generated internally.}

\item{kernel_continuous}{Kernel function for continuous predictors.}

\item{kernel_categorical}{Kernel function for categorical predictors.}

\item{kernel_levels}{Optional list of level vectors for each categorical predictor (needed for ordinal kernels).}

\item{estimator}{Either \code{"NW"} (default) or \code{"LL"}, indicating the type of regression estimator to use.}
}
\value{
A list with optimal \code{h_boot}, \code{lambda_boot}, and the minimum bootstrap CMISE.
}
\description{
Selects optimal bandwidths for nonparametric circular regression with mixed-type predictors,
by minimizing a bootstrap approximation of the Circular Mean Integrated Squared Error (CMISE).
}
\details{
Supports both Nadaraya-Watson (NW) and local linear (LL) estimators.
}
\note{
To ensure exact reproducibility across runs (e.g., when comparing to a parallel version),
you can precompute the bootstrap residuals externally with a fixed seed and pass them
via the \code{eps_bootstrap} argument as a matrix of size \verb{n x B}. Otherwise, the function
will sample residuals internally using \code{sample()}.
}
\examples{
set.seed(42)
n <- 20
x <- runif(n)
z <- sample(c("A", "B", "C"), n, replace = TRUE)
mu <- function(x, z) {
  if (z == "A") return(pi * x)
  if (z == "B") return(pi / 2 * x + pi / 4)
  return(pi * (1 - x))
}
theta <- sapply(1:n, function(i) {
  (mu(x[i], z[i]) + circular::rvonmises(1, mu = circular::circular(0), kappa = 5)) \%\% (2 * pi)
})
df <- data.frame(x = x, z = z, theta = theta)

result <- bootstrap_bandwidth_selector(
  data = df,
  response_col = "theta",
  cont_cols = "x",
  cat_cols = "z",
  h0 = 0.2, lambda0 = 0.4,
  h1 = 0.3, lambda1 = 0.4,
  estimator = "NW",
  B = 5,  # menor número de réplicas bootstrap
  kernel_continuous = gaussian_kernel,
  kernel_categorical = aitchison_aitken_kernel,
  kernel_levels = list(z = c("A", "B", "C"))
)
print(result)

}
