#' Compute Bootstrap Confidence Bands for Circular Regression (Nadaraya-Watson)
#'
#' Wrapper for \code{compute_bootstrap_bands_general()} using \code{nw_circular_predict}.
#'
#' 
#' @param ... Arguments passed to \code{compute_bootstrap_bands_general}.
#'
#' @return A data frame with columns: \code{x} (grid values), \code{estimate}, \code{lower}, \code{upper}.
#'
#' @export
compute_bootstrap_bands <- function(...) {
  compute_bootstrap_bands_general(..., predict_fun = nw_circular_predict)
}
