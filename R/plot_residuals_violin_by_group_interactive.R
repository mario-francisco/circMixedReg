#' Violin Plot of Residuals by Group (Interactive)
#'
#' Creates a violin plot of circular residuals grouped by a categorical variable.
#'
#' @param resids Vector of residuals (in radians).
#' @param groups Factor or character vector of group labels.
#' @param main Title of the plot.
#'
#' @return A ggplot2 object.
#' 
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme geom_violin geom_boxplot
#' @importFrom graphics title
#' @importFrom stats residuals runif
#' @importFrom circular circular
#'
#' @examples
#' set.seed(123)
#' resids <- runif(100, -pi, pi)
#' groups <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' plot_residuals_violin_by_group_interactive(resids, groups)
#'
#' @export
plot_residuals_violin_by_group_interactive <- function(resids, groups, main = "Violin Plot of Residuals by Group") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required. Please install it with install.packages('ggplot2').")
  }
  
  df <- data.frame(residuals = resids, group = as.factor(groups))
  
  ggplot(df, aes(x = group, y = residuals, fill = group)) +
     geom_violin(trim = FALSE, color = "gray30") +
     geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
     theme_minimal() +
     labs(title = main, y = "Circular Residuals (radians)", x = "Group") +
     theme(legend.position = "none")
}
