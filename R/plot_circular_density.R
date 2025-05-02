#' Plot Circular Density Estimates
#'
#' Plots kernel density estimates of circular data, optionally by group.
#'
#' @param angles Numeric vector of circular data (in radians).
#' @param groups Optional vector of group labels (factor or character).
#' @param main Title of the plot.
#' @param bw Bandwidth for circular kernel density estimation (passed to \code{density.circular}).
#' @param n_points Number of evaluation points on the circle.
#' @param show_uniform Logical. If TRUE, shows the uniform density as reference.
#'
#' @return A ggplot2 plot object.
#' 
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal scale_x_continuous geom_hline
#' @importFrom ggplot2 theme
#' @importFrom circular density.circular
#' @importFrom graphics title
#' @importFrom stats density
#' @importFrom circular circular rvonmises
#'
#' @examples
#' set.seed(123)
#' angles <- circular::rvonmises(200, mu = circular::circular(pi), kappa = 3)
#' groups <- sample(c("G1", "G2"), size = 200, replace = TRUE)
#'
#' # Plot without groups
#' plot_circular_density(angles)
#'
#' # Plot by groups
#' plot_circular_density(angles, groups)
#'
#' @export
plot_circular_density <- function(angles,
                                  groups = NULL,
                                  main = "Circular Density Plot",
                                  bw = 20,
                                  n_points = 512,
                                  show_uniform = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2.")
  if (!requireNamespace("circular", quietly = TRUE)) stop("Please install circular.")
  
  # Basic checks
  if (!is.numeric(angles)) stop("angles must be a numeric vector (in radians).")
  if (!is.null(groups) && length(groups) != length(angles)) stop("groups must have the same length as angles.")
  
  angles <- circular(angles, units = "radians", modulo = "2pi")
  
  if (is.null(groups)) {
    dens <- density.circular(angles, bw = bw, n = n_points)
    df <- data.frame(angle = as.numeric(dens$x), density = as.numeric(dens$y), group = "All")
  } else {
    groups <- as.factor(groups)
    df_list <- lapply(levels(groups), function(g) {
      dens <- density.circular(angles[groups == g], bw = bw, n = n_points)
      data.frame(angle = as.numeric(dens$x), density = as.numeric(dens$y), group = g)
    })
    df <- do.call(rbind, df_list)
  }
  
  p <- ggplot(df, aes(x = angle, y = density, color = group)) +
    geom_line(linewidth = 1.2) +
    scale_x_continuous(
      breaks = seq(0, 2 * pi, by = pi / 2),
      labels = c("0", expression(pi/2), expression(pi), expression(3*pi/2), expression(2*pi)),
      limits = c(0, 2 * pi)
    ) +
    labs(x = "Angle (radians)", y = "Density", title = main, color = if (!is.null(groups)) "Group" else NULL) +
    theme_minimal(base_size = 14)
  
  if (show_uniform) {
    p <- p + geom_hline(yintercept = 1 / (2 * pi), linetype = "dashed", color = "gray50")
  }
  
  return(p)
}
