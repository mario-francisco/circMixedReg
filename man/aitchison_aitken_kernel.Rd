% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aitchison_aitken_kernel.R
\name{aitchison_aitken_kernel}
\alias{aitchison_aitken_kernel}
\title{Aitchison-Aitken Kernel for Nominal Categorical Variables}
\usage{
aitchison_aitken_kernel(z, z_i, lambda, levels = NULL)
}
\arguments{
\item{z}{A character or factor level (target value).}

\item{z_i}{A character or factor level (observed value).}

\item{lambda}{Smoothing parameter (must be in \code{[0, 1]}).}

\item{levels}{Optional character vector of all possible levels. If \code{NULL}, it is inferred from \code{z} and \code{z_i}.}
}
\value{
A numeric kernel weight: \eqn{1 - \lambda} if \code{z == z_i}, and \eqn{\lambda / (k - 1)} otherwise,
where \eqn{k} is the number of levels.
}
\description{
Computes the Aitchison-Aitken kernel between two nominal categorical values.
This kernel assigns a higher weight to exact matches and a uniform weight
to mismatches, scaled by the smoothing parameter \code{lambda}.
}
\examples{
aitchison_aitken_kernel("A", "A", 0.2, levels = c("A", "B", "C"))  # Match
aitchison_aitken_kernel("A", "B", 0.2, levels = c("A", "B", "C"))  # Mismatch
aitchison_aitken_kernel("X", "Y", 0.5)                             # Inferred levels
aitchison_aitken_kernel("A", "B", 0, levels = c("A", "B", "C"))    # Kronecker delta
aitchison_aitken_kernel("A", "C", 1, levels = c("A", "B", "C"))    # Max smoothing

}
