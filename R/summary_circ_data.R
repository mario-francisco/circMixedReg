#' Summary and Visualization of Circular Data
#'
#' Provides descriptive statistics and graphical visualizations for a univariate circular variable.
#' Computes the number of observations, circular mean, circular median, concentration coefficient (rho),
#' and displays a scatterplot, rose diagram (histogram), nonparametric density estimate, violin plot, 
#' and circular boxplot (if \pkg{bpDir} is available).
#'
#' @param theta A numeric vector of angles in radians (can be of class `circular` or numeric).
#' @param plot Logical; if `TRUE`, graphical summaries are produced.
#' @param titles Logical; if `TRUE`, includes plot titles.
#'
#' @return A data frame with summary statistics: number of observations, mean, median, and concentration.
#'
#' @importFrom circular circular mean.circular median.circular rho.circular rose.diag
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_line geom_hline labs theme_minimal theme scale_x_continuous
#' @importFrom graphics plot symbols points text plot.new lines title
#' @importFrom graphics par
#'
#' @examples
#' set.seed(123)
#' theta <- circular::circular(runif(100, 0, 2 * pi))
#' summary_simple <- summary_circ_data(theta)
#' print(summary_simple)
#'
#' # To display graphical summaries
#' summary_circ_data(theta, plot = TRUE, titles = TRUE)
#'
#' @export
summary_circ_data <- function(theta, plot = TRUE, titles = TRUE) {
  if (!inherits(theta, "circular")) {
    theta <- circular(theta)
  }
  
  n <- length(theta)
  mean_dir <- mean.circular(theta)
  median_dir <- median.circular(theta)
  rho <- rho.circular(theta)
  
  # Organize output cleanly
  summary_table <- data.frame(
    n = n,
    mean_direction = as.numeric(mean_dir),
    median_direction = as.numeric(median_dir),
    concentration_rho = rho
  )
  
  if (plot) {
    # Scatterplot
    par(mar = c(1, 1, if (titles) 2.5 else 1, 1))  # More top margin if title
    plot(NA, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), asp = 1,
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n",
         main = if (titles) "" else "")
    if (titles) title("Circular Scatterplot", cex.main = 1)
    
    symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)
    angles <- seq(0, 2 * pi, by = pi/2)
    for (ang in angles) {
      lines(c(0, cos(ang)), c(0, sin(ang)), lty = 2, col = "gray60")
    }
    text(1.2, 0, "0", cex = 0.9)
    text(0, 1.2, expression(pi/2), cex = 0.9)
    text(-1.2, 0, expression(pi), cex = 0.9)
    text(0, -1.2, expression(3*pi/2), cex = 0.9)
    text(1.25, 0, expression(2*pi), cex = 0.7, pos = 4)
    points(cos(theta), sin(theta), pch = 19, col = "blue")
    
    # Histogram (Rose diagram)
    par(mar = c(1, 1, if (titles) 3.5 else 1.5, 1))
    rose.diag(theta, bins = 16, prop = 1.5, main = if (titles) "Circular Histogram" else "")
    
    # Density estimate (custom function)
    print(plot_circular_density(as.numeric(theta),
                                main = if (titles) "Circular Density" else "",
                                bw = 20, show_uniform = TRUE))
    
    # Violin plot
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      df <- data.frame(angle = as.numeric(theta))
      p <- ggplot(df, aes(x = "All", y = angle)) +
        geom_violin(fill = "skyblue", color = "black") +
        geom_boxplot(width = 0.1) +
        theme_minimal(base_size = 12) +
        labs(title = if (titles) "Violin Plot" else NULL, x = NULL, y = "Angle (radians)") +
        theme(plot.title = element_text(hjust = 0.5))
      print(p)
    }
    
    # Circular boxplot
    if (requireNamespace("bpDir", quietly = TRUE)) {
      bpDir::CircularBoxplot(theta)
      if (titles) title("Circular Boxplot", cex.main = 1)
    }
  }
  
  return(summary_table)
}
