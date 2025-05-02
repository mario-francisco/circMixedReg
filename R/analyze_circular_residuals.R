#' Analyze Residuals in Circular Regression
#'
#' Computes and visualizes diagnostic tools for circular regression residuals,
#' including CMASE_obs, circular R², histogram, residuals vs. fitted plot, and uniformity tests.
#' It also supports grouped visualizations (by category) using circular histograms,
#' violin plots, and circular boxplots, optionally saving them to PDF files.
#'
#' @param data Data frame with the sample.
#' @param response_col Name of the circular response column (in radians).
#' @param cont_cols Character vector of continuous predictor names.
#' @param cat_cols Character vector of categorical predictor names.
#' @param h Bandwidths for continuous variables.
#' @param lambda Smoothing parameters for categorical variables.
#' @param kernel_continuous Kernel function for continuous variables.
#' @param kernel_categorical Kernel function for categorical variables.
#' @param kernel_levels Optional list of levels (for ordinal kernels).
#' @param estimator Type of estimator: "NW" (Nadaraya–Watson) or "LL" (local linear). Default is "NW".
#' @param group_col Optional: name of column for grouping residuals by category.
#' @param bins Number of bins in the circular histogram (for global and group histograms).
#' @param circular_boxplot Logical. If TRUE, shows circular boxplots by group using bpDir.
#' @param violin_plot Logical. If TRUE, shows violin plots by group using ggplot2.
#' @param circular_boxplot_file Optional. If not NULL, saves circular boxplots to this PDF file.
#' @param violin_plot_file Optional. If not NULL, saves violin plot to this PDF file.
#' @param histogram_by_group Logical. If TRUE, shows circular histograms of residuals by group.
#' @param histogram_file Optional. If not NULL, saves histograms by group to this PDF file.
#' @param place Label placement for circular boxplot ("outside", "inside", "none").
#' @param units Units for circular boxplot labels ("radians" or "degrees").
#'
#' @return A list with residuals, fitted values, CMASE_obs, R²_circular and uniformity test results.
#'
#' @importFrom stats fitted residuals
#' @importFrom circular circular
#' 
#' @export
#'
#' @examples
#' # See examples in compute_residuals and predict_circular_mixed
analyze_circular_residuals <- function(data,
                                       response_col = "theta",
                                       cont_cols = NULL,
                                       cat_cols = NULL,
                                       h = 0.5,
                                       lambda = 0.5,
                                       kernel_continuous = gaussian_kernel,
                                       kernel_categorical = aitchison_aitken_kernel,
                                       kernel_levels = NULL,
                                       estimator = "NW",
                                       group_col = NULL,
                                       bins = 16,
                                       circular_boxplot = FALSE,
                                       violin_plot = FALSE,
                                       circular_boxplot_file = NULL,
                                       violin_plot_file = NULL,
                                       histogram_by_group = FALSE,
                                       histogram_file = NULL,
                                       place = "outside",
                                       units = "radians") {
  
  estimator <- match.arg(estimator, c("NW", "LL"))
  
  # Compute residuals and fitted values
  resids <- compute_residuals(data, response_col, cont_cols, cat_cols, h, lambda,
                              kernel_continuous, kernel_categorical, kernel_levels,
                              estimator = estimator)
  preds <- predict_circular_mixed(data, data, response_col, cont_cols, cat_cols,
                                  h, lambda, kernel_continuous, kernel_categorical, kernel_levels,
                                  estimator = estimator)
  theta <- circular(data[[response_col]], units = "radians", modulo = "2pi")
  
  # Compute global goodness-of-fit measures
  cmase_val <- compute_CMASE_obs(theta, preds)
  r2_val <- compute_R2_circular(theta, preds)
  
  cat("CMASE (obs):", round(cmase_val, 4), "\n")
  cat("R^2 (circular):", round(r2_val, 4), "\n\n")
  
  # --- Safe plot: restore layout to avoid overlapping from previous par(mfrow=...)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))  # Ensures original graphics settings are restored
  
  # Plot 1: Global circular histogram
  par(mfrow = c(1, 1))
  plot_residuals_histogram(resids, bins)
  
  # Plot 2: Residuals vs fitted values
  print(plot_residuals_vs_fitted(preds, resids))
  
  # Grouped diagnostic plots
  if (!is.null(group_col)) {
    group_vals <- data[[group_col]]
    
    if (histogram_by_group) {
      plot_residuals_histogram_by_group(resids, group_vals, bins = bins, file = histogram_file)
    }
    
    if (!is.null(violin_plot_file)) {
      plot_residuals_violin_by_group_pdf(resids, group_vals, file = violin_plot_file)
    } else if (violin_plot) {
      print(plot_residuals_violin_by_group_interactive(resids, group_vals))
    }
    
    if (!is.null(circular_boxplot_file)) {
      plot_circular_boxplot_by_group_pdf(resids, group_vals, file = circular_boxplot_file,
                                         place = place, units = units)
    } else if (circular_boxplot) {
      plot_circular_boxplot_by_group_interactive(resids, group_vals, place = place, units = units)
    }
  }
  
  # Uniformity tests (Watson U² and others)
  tests <- test_residuals_uniformity(resids)
  print(tests)
  
  return(invisible(list(
    residuals = resids,
    fitted = preds,
    CMASE_obs = cmase_val,
    R2_circular = r2_val,
    tests = tests
  )))
}
