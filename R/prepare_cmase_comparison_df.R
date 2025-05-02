#' Compute CMASE_obs by group for multiple selectors and prepare data for plotting
#'
#' This helper function computes CMASE_obs by group for each bandwidth selector (CV, bootstrap, RoT)
#' and returns a tidy data frame suitable for plotting or table export.
#'
#' @param theta Vector of observed angular responses
#' @param x Matrix or data frame of continuous predictors
#' @param z Categorical variable (factor or character) for grouping
#' @param condition_levels Character vector of levels in z (e.g., sensory conditions)
#' @param fit_fun_cv Function to predict with bandwidth selector CV
#' @param fit_fun_boot Function to predict with bandwidth selector Bootstrap
#' @param fit_fun_rot Function to predict with bandwidth selector Rule-of-thumb
#'
#' @return Data frame with columns: Condition, Selector, CMASE
#' 
#' @importFrom stats runif
#' @importFrom circular circular 

#' @export
#'
#' @examples
#' theta <- circular::circular(runif(30, 0, 2*pi))
#' x <- matrix(runif(30), ncol = 1)
#' z <- rep(c("A", "B", "C"), each = 10)
#' condition_levels <- c("A", "B", "C")
#' fit_dummy <- function(x, z) circular::circular(rep(pi, length(x)))
#' prepare_cmase_comparison_df(theta, x, z, condition_levels,
#'                              fit_dummy, fit_dummy, fit_dummy)
prepare_cmase_comparison_df <- function(theta, x, z, condition_levels,
                                        fit_fun_cv, fit_fun_boot, fit_fun_rot) {
  selectors <- c("CV", "Bootstrap", "Rule-of-thumb")
  fit_funs <- list(fit_fun_cv, fit_fun_boot, fit_fun_rot)
  
  cmase_data <- do.call(rbind, lapply(seq_along(selectors), function(j) {
    cmase_vals <- compute_CMASE_obs_by_group(theta, x, z, fit_funs[[j]], condition_levels)
    data.frame(
      Condition = condition_levels,
      Selector = selectors[j],
      CMASE = as.numeric(cmase_vals)
    )
  }))
  
  return(cmase_data)
}
