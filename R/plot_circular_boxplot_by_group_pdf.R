#' Save Circular Boxplots of Residuals by Group to PDF
#'
#' Saves one circular boxplot per group into a multipage PDF file.
#'
#' @param resids Vector of residuals (in radians).
#' @param groups Grouping variable (factor or character).
#' @param file Output PDF filename. Default: "circular_boxplots.pdf".
#' @param place Position of the angle labels: "outside", "inside" or "none".
#' @param units Units for the angle labels: "radians" (default) or "degrees".
#'
#' @return Invisibly returns the file path of the saved PDF.
#' 
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics title
#' @importFrom stats residuals runif
#' @importFrom circular circular 

#'
#' @examples
#' \dontrun{
#'   resids <- runif(90, -pi, pi)
#'   groups <- rep(c("A", "B", "C"), each = 30)
#'   plot_circular_boxplot_by_group_pdf(resids, groups, file = "residuals_boxplots.pdf")
#' }
#'
#' @export
plot_circular_boxplot_by_group_pdf <- function(resids,
                                               groups,
                                               file = "circular_boxplots.pdf",
                                               place = "outside",
                                               units = "radians") {
  if (!requireNamespace("bpDir", quietly = TRUE)) {
    stop("The 'bpDir' package is required. Install it with install.packages('bpDir').")
  }
  
  resids_clean <- as.numeric(resids)
  attributes(resids_clean) <- NULL
  groups <- as.factor(groups)
  
  pdf(file, width = 6, height = 6, onefile = TRUE)
  for (g in levels(groups)) {
    resids_g <- resids_clean[groups == g]
    resids_g_circ <- circular(resids_g, units = "radians", modulo = "2pi")
    bpDir::CircularBoxplot(resids_g_circ, template = NULL, place = place, units = units)
    title(paste("Group:", g))
  }
  dev.off()
  message("Multipage PDF saved to: ", normalizePath(file))
  invisible(file)
}
