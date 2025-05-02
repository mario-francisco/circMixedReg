#' Compute Simultaneous Bootstrap Confidence Bands (Nadaraya-Watson)
#'
#' Wrapper for \code{compute_simultaneous_bands_general()} using \code{nw_circular_predict}.
#'
#' @param ... Arguments passed to \code{compute_simultaneous_bands_general}.
#' @return Same as \code{compute_simultaneous_bands_general()}.
#' @export
compute_simultaneous_bands <- function(...) {
  compute_simultaneous_bands_general(..., predict_fun = nw_circular_predict)
}
