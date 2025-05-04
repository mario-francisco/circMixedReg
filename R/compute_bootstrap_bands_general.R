#' Compute Bootstrap Confidence Bands for Circular Regression (General Version)
#'
#' Computes pointwise bootstrap confidence bands for the estimated circular regression function
#' using a user-specified prediction function (e.g., Nadaraya-Watson or local linear).
#'
#' The plot includes two styles: "ribbon" (shaded band) and "points" (upper/lower points), with
#' optional directional arrows to represent circular intervals.
#'
#' @param data Data frame with the original sample.
#' @param x_grid Numeric vector. Values of the continuous predictor to compute the bands at.
#' @param z_level Character. Level of the categorical predictor to condition on.
#' @param h Bandwidth for the continuous predictor.
#' @param lambda Smoothing parameter for the categorical predictor.
#' @param B Integer. Number of bootstrap replications.
#' @param alpha Numeric. Significance level (e.g., 0.05 for 95% confidence bands).
#' @param cont_col Name of the continuous predictor column.
#' @param cat_col Name of the categorical predictor column.
#' @param response_col Name of the circular response variable (in radians).
#' @param kernel_continuous Kernel function for the continuous predictor.
#' @param kernel_categorical Kernel function for the categorical predictor.
#' @param kernel_levels Optional list of levels for ordinal kernels.
#' @param seed Optional integer. Random seed for reproducibility.
#' @param plot Logical. If TRUE, displays a ggplot2 visualization of the bands.
#' @param style Character. Either "points" or "ribbon" to choose visualization style.
#' @param show_estimate Logical. If TRUE, the estimated curve is shown in the plot.
#' @param arrows Logical. If TRUE and style = "points", directional arrows are drawn.
#' @param predict_fun Function. The circular prediction function to be used (e.g., \code{nw_circular_predict}, \code{ll_circular_predict}).
#'
#' @return A data frame with columns: \code{x} (grid values), \code{estimate}, \code{lower}, \code{upper}.
#'
#' @examples
#' \dontrun{
#' bands <- compute_bootstrap_bands_general(
#'   data = sim_data,
#'   x_grid = seq(0, 1, length.out = 50),
#'   z_level = "A",
#'   h = 0.2, lambda = 0.3,
#'   B = 100,
#'   plot = TRUE,
#'   style = "points",
#'   show_estimate = TRUE,
#'   arrows = TRUE,
#'   predict_fun = nw_circular_predict
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon geom_segment scale_y_continuous labs theme_minimal scale_color_manual scale_shape_manual geom_hline arrow
#' @importFrom stats quantile
#' @importFrom grid unit
#' @export
compute_bootstrap_bands_general <- function(data,
                                            x_grid,
                                            z_level,
                                            h,
                                            lambda,
                                            B = 200,
                                            alpha = 0.05,
                                            cont_col = "X",
                                            cat_col = "Z",
                                            response_col = "theta",
                                            kernel_continuous = gaussian_kernel,
                                            kernel_categorical = aitchison_aitken_kernel,
                                            kernel_levels = NULL,
                                            seed = NULL,
                                            plot = FALSE,
                                            style = c("points", "ribbon"),
                                            show_estimate = TRUE,
                                            arrows = TRUE,
                                            predict_fun = nw_circular_predict) {
  style <- match.arg(style)
  if (!is.null(seed)) set.seed(seed)
  
  z_levels <- levels(data[[cat_col]])
  if (!z_level %in% z_levels) stop("z_level must be one of the levels in the categorical predictor")
  
  new_data <- data.frame(
    setNames(list(x_grid, factor(z_level, levels = z_levels)), c(cont_col, cat_col))
  )
  
  m_hat <- predict_fun(data, new_data,
                       response_col = response_col,
                       cont_cols = cont_col,
                       cat_cols = cat_col,
                       h = h,
                       lambda = lambda,
                       kernel_continuous = kernel_continuous,
                       kernel_categorical = kernel_categorical,
                       kernel_levels = kernel_levels)
  
  m_boot <- replicate(B, {
    data_b <- data[sample(nrow(data), replace = TRUE), ]
    predict_fun(data_b, new_data,
                response_col = response_col,
                cont_cols = cont_col,
                cat_cols = cat_col,
                h = h,
                lambda = lambda,
                kernel_continuous = kernel_continuous,
                kernel_categorical = kernel_categorical,
                kernel_levels = kernel_levels)
  })
  
  delta <- (m_boot - matrix(m_hat, nrow = length(m_hat), ncol = B)) %% (2 * pi)
  delta <- (delta + pi) %% (2 * pi) - pi
  
  lower <- (m_hat + apply(delta, 1, quantile, probs = alpha / 2)) %% (2 * pi)
  upper <- (m_hat + apply(delta, 1, quantile, probs = 1 - alpha / 2)) %% (2 * pi)
  
  bands_df <- data.frame(
    x = x_grid,
    estimate = m_hat,
    lower = lower,
    upper = upper
  )
  
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {
    if (style == "points") {
      bands_df$wrap_case <- bands_df$upper < bands_df$lower
      bands_df$lower_plot <- bands_df$lower %% (2 * pi)
      bands_df$upper_plot <- bands_df$upper %% (2 * pi)
      df_normal <- bands_df[!bands_df$wrap_case, ]
      df_wrap_1 <- bands_df[bands_df$wrap_case, ]; df_wrap_1$y_start <- df_wrap_1$lower_plot; df_wrap_1$y_end <- 2 * pi
      df_wrap_2 <- bands_df[bands_df$wrap_case, ]; df_wrap_2$y_start <- 0; df_wrap_2$y_end <- df_wrap_2$upper_plot
    }
    
    p <- ggplot2::ggplot(bands_df, ggplot2::aes(x = x))
    
    if (style == "ribbon") {
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3)
    } else if (style == "points") {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(y = lower_plot, color = "Lower", shape = "Lower"), size = 2) +
        ggplot2::geom_point(ggplot2::aes(y = upper_plot, color = "Upper", shape = "Upper"), size = 2)
      
      if (arrows) {
        p <- p +
          ggplot2::geom_segment(data = df_normal,
                                ggplot2::aes(y = lower_plot, yend = upper_plot, x = x, xend = x),
                                arrow = ggplot2::arrow(length = grid::unit(0.12, "cm")),
                                color = "gray40", linewidth = 0.4) +
          ggplot2::geom_segment(data = df_wrap_1,
                                ggplot2::aes(y = y_start, yend = y_end, x = x, xend = x),
                                arrow = ggplot2::arrow(length = grid::unit(0.12, "cm")),
                                color = "gray40", linewidth = 0.4) +
          ggplot2::geom_segment(data = df_wrap_2,
                                ggplot2::aes(y = y_start, yend = y_end, x = x, xend = x),
                                arrow = ggplot2::arrow(length = grid::unit(0.12, "cm")),
                                color = "gray40", linewidth = 0.4)
      }
      
      p <- p +
        ggplot2::scale_color_manual(name = "Band Limit", values = c("Lower" = "red", "Upper" = "darkgreen")) +
        ggplot2::scale_shape_manual(name = "Band Limit", values = c("Lower" = 17, "Upper" = 18)) +
        ggplot2::geom_hline(yintercept = c(0, pi, 2 * pi), linetype = "dotted", color = "gray50", linewidth = 0.4)
    }
    
    if (show_estimate) {
      p <- p + ggplot2::geom_line(ggplot2::aes(y = estimate), color = "blue", linewidth = 1)
    }
    
    p <- p +
      ggplot2::labs(title = paste("Bootstrap Confidence Bands - Z =", z_level),
                    x = cont_col, y = "Angle (radians)") +
      ggplot2::scale_y_continuous(
        breaks = seq(0, 2 * pi, by = pi / 2),
        labels = c("0", expression(pi / 2), expression(pi), expression(3 * pi / 2), expression(2 * pi)),
        limits = c(0, 2 * pi)
      ) +
      ggplot2::theme_minimal(base_size = 14)
    
    print(p)
  }
  
  return(bands_df)
}

