#' Plot One or Multiple Circular Regression Curves
#'
#' Visualizes estimated circular regression curves over a grid of values. Optionally overlays observed data points,
#' multiple true regression functions (e.g., one per group), and supports plotting in a cylindrical coordinate layout.
#'
#' @param x_grid Numeric vector of grid values for the predictor.
#' @param m_hat_list A list of vectors of estimated circular values (in radians), each representing a curve.
#' @param labels Optional character vector with names for each estimated curve (used in the legend).
#' @param colors Optional character vector with colors for all curves (estimated + true). Defaults to black.
#' @param linetypes Optional character vector with line types (e.g., \code{"solid"}, \code{"dashed"}).
#' @param true_fun Optional. A single function that computes a true regression curve.
#' @param true_fun_list Optional. A list of true functions to draw multiple curves (e.g., one per group).
#' @param true_labels Optional. Labels for the true functions in the legend.
#' @param x_obs Optional. Vector of predictor values for observed data points.
#' @param theta_obs Optional. Vector of observed circular responses (in radians).
#' @param cylinder Logical. If \code{TRUE}, plots curves on a cylindrical (polar-y) coordinate system.
#' @param title Title of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis. Ignored if \code{cylinder = TRUE}.
#'
#' @return A \code{ggplot} object.
#' 
#' @importFrom circular rvonmises
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal
#' @importFrom ggplot2 scale_color_manual scale_linetype_manual coord_polar element_blank
#' @importFrom ggplot2 theme
#' 
#' @examples
#' # Simulated example: NW vs LL on a single group
#' set.seed(123)
#' n <- 100
#' x <- runif(n, -1, 1)
#' z <- factor(rep("A", n))
#' m_fun <- function(x) pi * sin(pi * x)
#' theta <- (m_fun(x) + circular::rvonmises(n, mu = circular::circular(0), kappa = 3)) %% (2 * pi)
#' 
#' # Grid for estimation
#' x_grid <- seq(-1, 1, length.out = 200)
#' z0 <- "A"
#'
#' # Estimate with NW and LL
#' m_hat_nw <- sapply(x_grid, function(x0)
#'   nw_circular_mixed(
#'     x0 = x0, z0 = z0,
#'     X = matrix(x, ncol = 1),
#'     Z = data.frame(z),
#'     theta = theta,
#'     h = 0.3, lambda = 0.3,
#'     kernel_continuous = gaussian_kernel,
#'     kernel_categorical = aitchison_aitken_kernel
#'   ))
#'
#' m_hat_ll <- sapply(x_grid, function(x0)
#'   ll_circular_mixed(
#'     x0 = x0, z0 = z0,
#'     X = matrix(x, ncol = 1),
#'     Z = data.frame(z),
#'     theta = theta,
#'     h = 0.3, lambda = 0.3,
#'     kernel_continuous = gaussian_kernel,
#'     kernel_categorical = aitchison_aitken_kernel
#'   ))
#'
#' # Plot both curves and the true function
#' plot_circular_regression(
#'   x_grid = x_grid,
#'   m_hat_list = list(NW = m_hat_nw, LL = m_hat_ll),
#'   labels = c("NW", "LL"),
#'   colors = c("red", "blue"),
#'   linetypes = c("solid", "dotted"),
#'   true_fun = m_fun,
#'   x_obs = x,
#'   theta_obs = theta,
#'   title = "Estimated Circular Regression Curves"
#' )
#' @export
plot_circular_regression <- function(x_grid, 
                                     m_hat_list, 
                                     labels = NULL,
                                     colors = NULL,
                                     linetypes = NULL,
                                     true_fun = NULL,
                                     true_fun_list = NULL,
                                     true_labels = NULL,
                                     x_obs = NULL, 
                                     theta_obs = NULL,
                                     cylinder = FALSE,
                                     title = "Circular Regression Curve",
                                     xlab = "x", 
                                     ylab = "Estimated angle (radians)") {
  
  # Estimated curves
  n_curves <- length(m_hat_list)
  if (is.null(labels)) labels <- paste("Curve", 1:n_curves)
  if (is.null(colors)) colors <- rep("black", n_curves)
  if (is.null(linetypes)) linetypes <- rep("solid", n_curves)
  
  df_list <- lapply(1:n_curves, function(i) {
    data.frame(x = x_grid, 
               angle = m_hat_list[[i]] %% (2 * pi), 
               curve = labels[i])
  })
  df_all <- do.call(rbind, df_list)
  
  # Base plot
  p <- ggplot(df_all, aes(x = x, y = angle, color = curve, linetype = curve)) +
    geom_line(linewidth = 1.2, na.rm = TRUE)
  
  # Observed points
  if (!is.null(x_obs) & !is.null(theta_obs)) {
    p <- p + geom_point(data = data.frame(x = x_obs, theta = theta_obs),
                        aes(x = x, y = theta), 
                        inherit.aes = FALSE,
                        color = "gray40", alpha = 0.6, size = 1.5)
  }
  
  # Single true curve
  if (!is.null(true_fun)) {
    df_true <- data.frame(x = x_grid, 
                          angle = true_fun(x_grid) %% (2 * pi),
                          curve = "True")
    p <- p + geom_line(data = df_true,
                       aes(x = x, y = angle, color = curve, linetype = curve),
                       linewidth = 1)
    labels <- c(labels, "True")
    colors <- c(colors, "blue")
    linetypes <- c(linetypes, "dashed")
  }
  
  # Multiple true curves
  if (!is.null(true_fun_list)) {
    if (is.null(true_labels)) true_labels <- paste0("True_", seq_along(true_fun_list))
    true_df <- do.call(rbind, lapply(seq_along(true_fun_list), function(i) {
      data.frame(x = x_grid,
                 angle = true_fun_list[[i]](x_grid) %% (2 * pi),
                 curve = true_labels[i])
    }))
    p <- p + geom_line(data = true_df,
                       aes(x = x, y = angle, color = curve, linetype = curve),
                       linewidth = 1)
    labels <- c(labels, true_labels)
    colors <- c(colors, rep("black", length(true_fun_list)))  # You can customize this
    linetypes <- c(linetypes, rep("dashed", length(true_fun_list)))
  }
  
  # Axis style
  p <- p + 
    scale_y_continuous(
      breaks = seq(0, 2 * pi, by = pi / 2),
      labels = c("0", expression(pi/2), expression(pi), expression(3*pi/2), expression(2*pi)),
      limits = c(0, 2 * pi)
    ) +
    labs(title = title, x = xlab, y = ylab, color = "Curve", linetype = "Curve") +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = linetypes)
  
  if (cylinder) {
    p <- p + coord_polar(theta = "y", start = 0, direction = 1) +
      labs(y = "Angle (rad)", x = "") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank())
  }
  
  return(p)
}

# Optional visualization (not essential for core package)
# Requires 'plotly' to be installed
# plot_circular_regression_3D <- function(...) { ... }
