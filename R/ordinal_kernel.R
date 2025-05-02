#' Ordinal Kernel with Explicit Level Handling
#'
#' Computes the value of an ordinal kernel between two categorical values \code{z} and \code{z_i},
#' where the similarity decays geometrically with the absolute distance between their positions
#' in a user-defined ordered scale.
#'
#' This kernel is particularly useful when the categorical variable has an intrinsic order
#' (e.g., "Low", "Medium", "High") and you want smoother transitions between adjacent levels.
#'
#' @param z A factor or character scalar representing the target level.
#' @param z_i A factor or character scalar representing the data level to compare with \code{z}.
#' @param lambda A smoothing parameter in the interval (0, 1). Higher values yield sharper decay.
#' @param levels A character vector with the full ordered list of levels (e.g., \code{c("Low", "Medium", "High")}).
#'
#' @return A scalar kernel weight in \eqn{(0, 1]}.
#' Returns \code{0} and a warning if either level is not present in the specified \code{levels}.
#'
#' @examples
#' # Define an ordinal scale
#' L <- c("Low", "Medium", "High")
#'
#' # Same level → weight = 1
#' ordinal_kernel("Low", "Low", lambda = 0.3, levels = L)
#'
#' # One level apart → weight = (1 - lambda)^1
#' ordinal_kernel("Low", "Medium", lambda = 0.3, levels = L)
#'
#' # Two levels apart → weight = (1 - lambda)^2
#' ordinal_kernel("Low", "High", lambda = 0.3, levels = L)
#'
#' # If level not in `levels` → warning and 0
#' ordinal_kernel("Low", "Extreme", lambda = 0.3, levels = L)
#'
#' @export
ordinal_kernel <- function(z, z_i, lambda, levels) {
  # Validate lambda
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda <= 0 || lambda >= 1) {
    stop("ordinal_kernel: 'lambda' must be a number in (0, 1).")
  }
  
  # Check levels
  if (missing(levels) || is.null(levels)) {
    stop("ordinal_kernel: 'levels' must be provided explicitly.")
  }
  
  # Coerce to character for consistency
  z <- as.character(z)[1]
  z_i <- as.character(z_i)[1]
  levels <- as.character(levels)
  
  # Get numeric positions
  z_num <- match(z, levels)
  z_i_num <- match(z_i, levels)
  
  # Handle invalid levels gracefully
  if (is.na(z_num) || is.na(z_i_num)) {
    warning(sprintf("Level '%s' or '%s' not found in ordinal_kernel(): returning 0", z, z_i))
    return(0)
  }
  
  # Compute distance and kernel value
  d <- abs(z_num - z_i_num)
  return((1 - lambda)^d)
}
