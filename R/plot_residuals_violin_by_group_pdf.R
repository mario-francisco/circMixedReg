#' Save Violin Plot of Residuals by Group to PDF
#'
#' Creates and saves a violin plot of circular residuals grouped by a categorical variable.
#'
#' @param resids Vector of residuals (in radians).
#' @param groups Factor or character vector of group labels.
#' @param file Output PDF file name.
#' @param main Title of the plot.
#'
#' @return Invisibly returns the path to the saved PDF file.
#' 
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme geom_violin geom_boxplot
#' @importFrom grDevices pdf
#' @importFrom graphics title
#' @importFrom stats residuals runif
#' @importFrom circular circular
#' @importFrom ggplot2 ggsave
#'
#' @examples
#' set.seed(123)
#' resids <- runif(100, -pi, pi)
#' groups <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' plot_file <- tempfile(fileext = ".pdf")
#' plot_residuals_violin_by_group_pdf(resids, groups, file = plot_file)
#'
#' @export
plot_residuals_violin_by_group_pdf <- function(resids, groups, file = "violin_residuals.pdf",
                                               main = "Violin Plot of Residuals by Group") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required. Please install it.")
  }
  
  df <- data.frame(residuals = resids, group = as.factor(groups))
  
  p <-  ggplot(df,  aes(x = group, y = residuals, fill = group)) +
     geom_violin(trim = FALSE, color = "gray30") +
     geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
     theme_minimal() +
     labs(title = main, y = "Circular Residuals (radians)", x = "Group") +
     theme(legend.position = "none")
  
   ggsave(filename = file, plot = p, width = 6, height = 4)
  message("Violin plot saved to: ", normalizePath(file))
  invisible(file)
}
