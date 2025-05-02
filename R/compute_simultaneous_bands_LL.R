#' Compute Simultaneous Bootstrap Confidence Bands (Local Linear)
#'
#' Wrapper for \code{compute_simultaneous_bands_general()} using \code{ll_circular_predict}.
#'
#' @param ... Arguments passed to \code{compute_simultaneous_bands_general}.
#' @return Same as \code{compute_simultaneous_bands_general()}.
#' @export
compute_simultaneous_bands_LL <- function(...) {
  compute_simultaneous_bands_general(..., predict_fun = ll_circular_predict)
}
