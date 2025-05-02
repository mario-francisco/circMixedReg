#' Plot Circular Boxplots of Residuals by Group (Interactive)
#'
#' Creates one circular boxplot per group and displays them interactively
#' using the `bpDir::CircularBoxplot` function.
#'
#' @param resids Vector of residuals (in radians).
#' @param groups Grouping variable (factor or character).
#' @param place Position of the angle labels: "outside", "inside" or "none".
#' @param units Units for the angle labels: "radians" (default) or "degrees".
#'
#' @return No return value; generates plots as side effects.
#' 
#' @importFrom graphics title
#' @importFrom stats residuals runif
#' @importFrom circular circular 
#'
#' @examples
#' \dontrun{
#'   # Simulate example
#'   set.seed(1)
#'   resids <- runif(100, -pi, pi)
#'   groups <- sample(c("A", "B", "C"), 100, replace = TRUE)
#'   plot_circular_boxplot_by_group_interactive(resids, groups)
#' }
#'
#' @export
plot_circular_boxplot_by_group_interactive <- function(resids,
                                                       groups,
                                                       place = "outside",
                                                       units = "radians") {
  if (!requireNamespace("bpDir", quietly = TRUE)) {
    stop("The 'bpDir' package is required. Install it with install.packages('bpDir').")
  }
  
  resids_clean <- as.numeric(resids)
  attributes(resids_clean) <- NULL
  groups <- as.factor(groups)
  
  for (g in levels(groups)) {
    resids_g <- resids_clean[groups == g]
    resids_g_circ <- circular(resids_g, units = "radians", modulo = "2pi")
    bpDir::CircularBoxplot(resids_g_circ, template = NULL, place = place, units = units)
    title(paste("Group:", g))
  }
}
