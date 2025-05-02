#' Plot Circular Histogram of Residuals by Group (one per plot)
#'
#' Displays one circular rose diagram per group in separate figures, allowing
#' visual inspection of the residual distribution by subgroup.
#'
#' @param resids Vector of residuals (in radians).
#' @param groups Grouping variable (factor or character).
#' @param bins Number of bins in the rose diagram.
#' @param file Optional: name of PDF file to save. If NULL, plots are shown interactively.
#' @param shrink Shrink factor for the circular diagram.
#'
#' @return Creates plots (side effect). Returns \code{NULL} invisibly.
#' 
#' @importFrom grDevices pdf dev.off
#' @importFrom stats residuals runif
#' @importFrom circular rose.diag
#'
#' @examples
#' # Simulated residuals and groups
#' set.seed(123)
#' resids <- runif(100, -pi, pi)
#' groups <- sample(c("A", "B", "C"), 100, replace = TRUE)
#'
#' # Interactive plot
#' plot_residuals_histogram_by_group(resids, groups)
#'
#' # Save to PDF
#' # plot_residuals_histogram_by_group(resids, groups, file = "residual_histograms.pdf")
#'
#' @export
plot_residuals_histogram_by_group <- function(resids,
                                              groups,
                                              bins = 16,
                                              file = NULL,
                                              shrink = 1.3) {
  if (!requireNamespace("circular", quietly = TRUE)) {
    stop("The 'circular' package is required. Please install it.")
  }
  
  resids_clean <- as.numeric(resids)
  attributes(resids_clean) <- NULL
  groups <- as.factor(groups)
  
  if (!is.null(file)) {
    pdf(file, width = 6, height = 6, onefile = TRUE)
  }
  
  for (g in levels(groups)) {
    resids_g <- resids_clean[groups == g]
    resids_g_circ <- circular(resids_g, units = "radians", modulo = "2pi")
    rose.diag(resids_g_circ, bins = bins, shrink = shrink, main = paste("Residuals - Group", g))
  }
  
  if (!is.null(file)) {
    dev.off()
    message("Histograms saved to: ", normalizePath(file))
  }
  
  invisible(NULL)
}
