#' Plot Residuals vs. Fitted Values (Cartesian)
#'
#' Creates a standard scatter plot of circular residuals versus fitted values,
#' both expressed in radians. This diagnostic plot helps identify systematic
#' patterns or heteroscedasticity in circular regression models.
#'
#' @param fitted_vals Numeric vector of fitted circular values (in radians).
#' @param resids Numeric vector of residuals (in radians).
#' @param main Optional character string specifying the plot title.
#'
#' @return A \code{ggplot2} object displaying the scatter plot.
#' 
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal scale_x_continuous geom_hline
#' @importFrom graphics title
#' @importFrom stats fitted residuals runif
#' @importFrom circular circular
#' @importFrom ggplot2 theme
#'
#' @examples
#' fitted_vals <- runif(100, 0, 2 * pi)
#' resids <- (rnorm(100, 0, 0.5) + pi) %% (2 * pi) - pi
#' plot_residuals_vs_fitted(fitted_vals, resids)
#'
#' @export
plot_residuals_vs_fitted <- function(fitted_vals, resids, main = "Residuals vs. Fitted Values") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2.")
  }
  
  df <- data.frame(
    fitted = fitted_vals %% (2 * pi),
    residuals = resids
  )
  
   ggplot(df,  aes(x = fitted, y = residuals)) +
     geom_point(color = "blue", alpha = 0.6) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
     scale_x_continuous(
      breaks = seq(0, 2 * pi, by = pi / 2),
      labels = c("0", expression(pi/2), expression(pi), expression(3*pi/2), expression(2*pi)),
      limits = c(0, 2 * pi)
    ) +
     labs(
      title = main,
      x = "Fitted Values (radians)",
      y = "Residuals (radians)"
    ) +
     theme_minimal(base_size = 14)
}
