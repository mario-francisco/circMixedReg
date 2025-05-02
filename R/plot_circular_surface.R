#' Plot Circular Regression Surface for Two Continuous Predictors
#'
#' This function visualizes the estimated circular regression function
#' over a two-dimensional grid of continuous predictor values.
#' The angular output is encoded using a perceptually uniform circular color scale.
#'
#' It is intended for use with circular nonparametric regression models 
#' involving two continuous covariates and one categorical factor (held fixed).
#' The resulting heatmap reveals how directional responses vary across
#' the joint domain of the two predictors.
#'
#' @param x_grid Numeric vector. Grid values for the first continuous predictor (e.g., actual distance).
#' @param y_grid Numeric vector. Grid values for the second continuous predictor (e.g., distance error).
#' @param z_matrix Numeric matrix. Estimated circular responses (in radians),
#'                 of size \code{length(y_grid)} x \code{length(x_grid)}.
#' @param title Optional string. Main title of the plot.
#' @param xlab Optional string. Label for the x-axis.
#' @param ylab Optional string. Label for the y-axis.
#'
#' @return A \code{ggplot2} object displaying a circular-valued surface as a heatmap.
#' 
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text geom_raster scale_fill_gradientn coord_fixed
#' @importFrom grDevices hcl.colors
#' @importFrom graphics title
#' @importFrom circular circular 
#'
#' @examples
#' x_grid <- seq(-1, 1, length.out = 50)
#' y_grid <- seq(-1, 1, length.out = 50)
#' m_fun <- function(x, y) atan2(sin(pi * x * y), cos(pi * x * y)) %% (2 * pi)
#' m_grid <- outer(x_grid, y_grid, Vectorize(function(x, y) m_fun(x, y)))
#' plot_circular_surface(x_grid, y_grid, m_grid,
#'                       xlab = "Predictor 1",
#'                       ylab = "Predictor 2",
#'                       title = "Synthetic Circular Surface")
#' @export
plot_circular_surface <- function(x_grid, y_grid, z_matrix,
                                  title = "Circular Regression Surface",
                                  xlab = "Predictor 1", ylab = "Predictor 2") {
  
  # Create dataframe for ggplot
  df <- expand.grid(x = x_grid, y = y_grid)
  df$angle <- as.vector(z_matrix %% (2 * pi))  # Ensure values are in [0, 2pi)
  
  # Use smooth Viridis colors for circular data
  n_colors <- 100
  color_map <- hcl.colors(n = n_colors, palette = "Viridis", rev = FALSE)
  
  ggplot(df, aes(x = x, y = y, fill = angle)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradientn(
      colours = color_map,
      limits = c(0, 2 * pi),
      breaks = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
      labels = c("0", expression(pi/2), expression(pi), expression(3*pi/2), expression(2*pi)),
      name = "Angle (rad)"
    ) +
    coord_fixed() +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_text(size = 12),
          legend.text = element_text(size = 11))
}
