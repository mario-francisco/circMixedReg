#' Summary and Visualization of Circular Data by Groups
#'
#' Provides descriptive statistics and graphical visualizations for a circular variable across groups.
#' For each group, computes the number of observations, circular mean, circular median, and concentration coefficient (rho).
#' Displays scatterplots, rose diagrams, nonparametric density estimates, violin plots, and circular boxplots (if \pkg{bpDir} is available) separately for each group.
#'
#' @param theta A numeric vector of angles in radians (can be of class `circular` or numeric).
#' @param group A factor or character vector indicating group membership.
#' @param plot Logical; if `TRUE`, graphical summaries are produced individually per group.
#' @param titles Logical; if `TRUE`, plot titles are shown.
#'
#' @return A data frame with summary statistics for each group.
#'
#' @importFrom circular circular mean.circular median.circular rho.circular rose.diag
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_line geom_hline labs theme_minimal theme scale_x_continuous
#' @importFrom graphics plot symbols points text plot.new lines title
#' @importFrom graphics par
#'
#' @examples
#' set.seed(123)
#' theta <- circular::circular(runif(100, 0, 2 * pi))
#' group <- sample(c("A", "B"), size = 100, replace = TRUE)
#' summary_groups <- summary_circ_data_by_group(theta, group)
#' print(summary_groups)
#'
#' # To display graphical summaries by group
#' summary_circ_data_by_group(theta, group, plot = TRUE, titles = TRUE)
#'
#' @export
summary_circ_data_by_group <- function(theta, group, plot = TRUE, titles = TRUE) {
  if (!inherits(theta, "circular")) {
    theta <- circular(theta)
  }
  if (!is.factor(group)) {
    group <- as.factor(group)
  }
  
  results <- list()
  
  for (g in levels(group)) {
    idx <- group == g
    theta_g <- theta[idx]
    
    n <- length(theta_g)
    mean_dir <- mean.circular(theta_g)
    median_dir <- median.circular(theta_g)
    rho <- rho.circular(theta_g)
    
    results[[g]] <- data.frame(
      group = g,
      n = n,
      mean_direction = as.numeric(mean_dir),
      median_direction = as.numeric(median_dir),
      concentration_rho = rho
    )
    
    if (plot) {
      # Scatterplot
      par(mar = c(1, 1, if (titles) 2.5 else 1, 1))
      plot(NA, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), asp = 1,
           xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
      if (titles) title(paste("Circular Scatterplot - Group", g), cex.main = 1)
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
      points(cos(theta_g), sin(theta_g), pch = 19, col = "blue")
      
      # Rose diagram
      par(mar = c(1, 1, if (titles) 3.5 else 1.5, 1))
      rose.diag(theta_g, bins = 16, prop = 1.5,
                main = if (titles) paste("Circular Histogram - Group", g) else "")
      
      # Density plot
      print(plot_circular_density(as.numeric(theta_g),
                                  main = if (titles) paste("Circular Density - Group", g) else "",
                                  bw = 20, show_uniform = TRUE))
      
      # Violin plot
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        df <- data.frame(angle = as.numeric(theta_g))
        p <- ggplot(df, aes(x = g, y = angle)) +
          geom_violin(fill = "lightgreen", color = "black") +
          geom_boxplot(width = 0.1) +
          theme_minimal(base_size = 12) +
          labs(title = if (titles) paste("Violin Plot - Group", g) else NULL,
               x = "", y = "Angle (radians)") +
          theme(plot.title = element_text(hjust = 0.5))
        print(p)
      }
      
      # Circular boxplot
      if (requireNamespace("bpDir", quietly = TRUE)) {
        bpDir::CircularBoxplot(theta_g)
        if (titles) title(paste("Circular Boxplot - Group", g), cex.main = 1)
      }
    }
  }
  
  output <- do.call(rbind, results)
  invisible(output)
}
