#' Aitchison-Aitken Kernel for Nominal Categorical Variables
#'
#' Computes the Aitchison-Aitken kernel between two nominal categorical values.
#' This kernel assigns a higher weight to exact matches and a uniform weight
#' to mismatches, scaled by the smoothing parameter \code{lambda}.
#'
#' @param z A character or factor level (target value).
#' @param z_i A character or factor level (observed value).
#' @param lambda Smoothing parameter (must be in \code{[0, 1]}).
#' @param levels Optional character vector of all possible levels. If \code{NULL}, it is inferred from \code{z} and \code{z_i}.
#'
#' @return A numeric kernel weight: \eqn{1 - \lambda} if \code{z == z_i}, and \eqn{\lambda / (k - 1)} otherwise,
#'         where \eqn{k} is the number of levels.
#'
#' @examples
#' aitchison_aitken_kernel("A", "A", 0.2, levels = c("A", "B", "C"))  # Match
#' aitchison_aitken_kernel("A", "B", 0.2, levels = c("A", "B", "C"))  # Mismatch
#' aitchison_aitken_kernel("X", "Y", 0.5)                             # Inferred levels
#' aitchison_aitken_kernel("A", "B", 0, levels = c("A", "B", "C"))    # Kronecker delta
#' aitchison_aitken_kernel("A", "C", 1, levels = c("A", "B", "C"))    # Max smoothing
#'
#' @export
aitchison_aitken_kernel <- function(z, z_i, lambda, levels = NULL) {
  # Validate lambda
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda < 0 || lambda > 1) {
    stop("aitchison_aitken_kernel: 'lambda' must be a number in [0, 1].")
  }
  
  # Coerce inputs to character
  z <- as.character(z)[1]
  z_i <- as.character(z_i)[1]
  
  # Determine levels if not provided
  if (is.null(levels)) {
    levels <- sort(unique(c(z, z_i)))
  } else {
    levels <- as.character(levels)
  }
  
  # Validate levels
  if (!(z %in% levels) || !(z_i %in% levels)) {
    warning(sprintf("Level '%s' or '%s' not found in aitchison_aitken_kernel(): returning 0", z, z_i))
    return(0)
  }
  
  k <- length(levels)
  if (k <= 1) {
    warning("aitchison_aitken_kernel: Number of levels must be greater than 1. Returning 0.")
    return(0)
  }
  
  # Compute kernel weight
  if (z == z_i) {
    return(1 - lambda)
  } else {
    return(lambda / (k - 1))
  }
}
