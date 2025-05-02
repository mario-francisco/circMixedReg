# R/globals.R

# Declare global variables to avoid NOTES in R CMD check
utils::globalVariables(c(
  "x", "y", "angle", "group", "estimate", "lower", "upper",
  "Condition", "CMASE", "Selector", "curve", "theta", "plot_residuals_by_group",
  "lower_plot", "upper_plot", "y_start", "y_end"
))