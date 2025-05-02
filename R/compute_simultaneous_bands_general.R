#' Compute Simultaneous Bootstrap Confidence Bands for Circular Regression (General Version)
#'
#' Constructs approximate simultaneous (1 - alpha) confidence bands for a circular regression function,
#' using bootstrap calibration between individual and Bonferroni bands.
#'
#' The function automatically decides whether to apply the iterative procedure based on the coverage
#' achieved by Bonferroni bands. Two visualization styles are available: "points" (with optional arrows)
#' and "ribbon" (traditional band). For circular data, the "points" style is often more appropriate.
#'
#' @param data Data frame containing the observed sample.
#' @param x_grid Numeric vector of values of the continuous predictor at which to evaluate the bands.
#' @param z_level Character; level of the categorical predictor to condition on.
#' @param h Bandwidth for the continuous predictor.
#' @param lambda Smoothing parameter for the categorical predictor.
#' @param B Number of bootstrap replicates. Default is 200.
#' @param alpha Nominal significance level (e.g., 0.05 for 95% bands).
#' @param delta Tolerance for the final coverage approximation. Default is \code{alpha / 10}.
#' @param max_iter Maximum number of calibration iterations. Default is 20.
#' @param cont_col Name of the continuous predictor column.
#' @param cat_col Name of the categorical predictor column.
#' @param response_col Name of the circular response column (in radians).
#' @param kernel_continuous Kernel function for the continuous predictor.
#' @param kernel_categorical Kernel function for the categorical predictor.
#' @param kernel_levels Optional list of levels for ordinal kernels.
#' @param seed Optional integer seed for reproducibility.
#' @param plot Logical. If TRUE, produces a ggplot2 visualization of the bands.
#' @param style Character. Either "points" or "ribbon" to control the plot appearance.
#' @param show_estimate Logical. If TRUE, adds the estimated regression curve to the plot.
#' @param arrows Logical. If TRUE and style = "points", adds directional arrows to indicate the interval span.
#' @param predict_fun Prediction function to use (e.g., \code{nw_circular_predict}, \code{ll_circular_predict}).
#'
#' @return A list with components:
#' \describe{
#'   \item{x}{Grid of x values}
#'   \item{estimate}{Estimated regression function}
#'   \item{lower}{Lower simultaneous confidence band}
#'   \item{upper}{Upper simultaneous confidence band}
#'   \item{alpha_final}{Final calibrated level used for band construction}
#'   \item{coverage}{Bootstrap proportion of curves entirely inside the band}
#'   \item{iterations}{Number of iterations used}
#'   \item{method}{"bonferroni" or "iterative" depending on which was applied}
#' }
#'
#' @examples
#' # Example (assuming data is preloaded with columns X, Z, theta):
#' # compute_simultaneous_bands_general(data, x_grid = seq(0,1,length.out=100),
#' #                                    z_level = "cond1", h = 0.2, lambda = 0.5,
#' #                                    plot = TRUE, style = "points",
#' #                                    show_estimate = TRUE, arrows = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon geom_segment labs theme_minimal scale_y_continuous scale_color_manual scale_shape_manual geom_hline arrow
#' @importFrom stats quantile
#' @importFrom grid unit
#' @export
compute_simultaneous_bands_general <- function(data,
                                               x_grid,
                                               z_level,
                                               h,
                                               lambda,
                                               B = 200,
                                               alpha = 0.05,
                                               delta = alpha / 10,
                                               max_iter = 20,
                                               cont_col = "X",
                                               cat_col = "Z",
                                               response_col = "theta",
                                               kernel_continuous = gaussian_kernel,
                                               kernel_categorical = aitchison_aitken_kernel,
                                               kernel_levels = NULL,
                                               seed = NULL,
                                               plot = FALSE,
                                               style = c("points", "ribbon"),
                                               show_estimate = FALSE,
                                               arrows = TRUE,
                                               predict_fun = nw_circular_predict) {
  style <- match.arg(style)
  if (!is.null(seed)) set.seed(seed)
  
  z_levels <- levels(data[[cat_col]])
  if (!z_level %in% z_levels) stop("Invalid z_level")
  
  new_data <- data.frame(
    x_val = x_grid,
    z_val = factor(z_level, levels = z_levels)
  )
  names(new_data) <- c(cont_col, cat_col)
  
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
  
  delta_mat <- (m_boot - matrix(m_hat, nrow = length(m_hat), ncol = B)) %% (2 * pi)
  delta_mat <- (delta_mat + pi) %% (2 * pi) - pi
  
  G <- length(x_grid)
  alpha_bonf <- alpha / G
  q_low <- apply(delta_mat, 1, quantile, probs = alpha_bonf / 2)
  q_high <- apply(delta_mat, 1, quantile, probs = 1 - alpha_bonf / 2)
  lower_band <- (m_hat + q_low) %% (2 * pi)
  upper_band <- (m_hat + q_high) %% (2 * pi)
  
  m_boot_i <- (matrix(m_hat, nrow = G, ncol = B) + delta_mat) %% (2 * pi)
  in_circ_interval <- function(theta, low, up) {
    theta <- theta %% (2 * pi); low <- low %% (2 * pi); up <- up %% (2 * pi)
    ifelse(low <= up, theta >= low & theta <= up, theta >= low | theta <= up)
  }
  check_coverage <- function(mb, low, up) {
    apply(mb, 2, function(curve) all(in_circ_interval(curve, low, up)))
  }
  covered <- check_coverage(m_boot_i, lower_band, upper_band)
  coverage <- mean(covered)
  
  method_used <- "bonferroni"
  alpha_final <- alpha_bonf
  k <- 0
  
  if (coverage > (1 - alpha)) {
    method_used <- "iterative"
    alpha_low <- alpha_bonf
    alpha_high <- alpha
    repeat {
      alpha_mean <- (alpha_low + alpha_high) / 2
      q_low <- apply(delta_mat, 1, quantile, probs = alpha_mean / 2)
      q_high <- apply(delta_mat, 1, quantile, probs = 1 - alpha_mean / 2)
      lower_band <- (m_hat + q_low) %% (2 * pi)
      upper_band <- (m_hat + q_high) %% (2 * pi)
      covered <- check_coverage(m_boot_i, lower_band, upper_band)
      coverage <- mean(covered)
      if (abs(coverage - (1 - alpha)) < delta || k >= max_iter) break
      if (coverage >= 1 - alpha) alpha_low <- alpha_mean else alpha_high <- alpha_mean
      k <- k + 1
    }
    alpha_final <- alpha_mean
  }
  
  bands_df <- data.frame(
    x = x_grid,
    estimate = m_hat,
    lower = lower_band,
    upper = upper_band
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
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "pink", alpha = 0.4)
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
      ggplot2::labs(title = paste("Simultaneous Bootstrap Bands (Circular Scale) - Z =", z_level),
                    x = cont_col, y = "Angle (radians)") +
      ggplot2::scale_y_continuous(
        breaks = seq(0, 2 * pi, by = pi / 2),
        labels = c("0", expression(pi/2), expression(pi), expression(3*pi/2), expression(2*pi)),
        limits = c(0, 2 * pi)
      ) +
      ggplot2::theme_minimal(base_size = 14)
    
    print(p)
  }
  
  return(list(
    x = x_grid,
    estimate = m_hat,
    lower = lower_band,
    upper = upper_band,
    alpha_final = alpha_final,
    coverage = coverage,
    iterations = k,
    method = method_used
  ))
}




