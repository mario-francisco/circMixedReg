# plot_cmase.R - Functions to visualize CMASE by group and selector

#' Plot CMASE_obs by condition and selector
#'
#' This function takes a data frame of CMASE values by condition and selector
#' and creates a grouped barplot for visual comparison.
#'
#' @param cmase_data Data frame with columns: Condition, Selector, CMASE
#'
#' @return A ggplot2 object
#' 
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text geom_bar position_dodge scale_fill_manual
#' @importFrom graphics title
#' @importFrom stats runif
#' @importFrom circular circular 
#' 
#' @export
#'
#' @examples
#' cmase_data <- data.frame(
#'   Condition = rep(c("A", "B", "C"), 3),
#'   Selector = rep(c("CV", "Bootstrap", "Rule-of-thumb"), each = 3),
#'   CMASE = runif(9, 0, 1)
#' )
#' plot_circular_cmase_by_condition(cmase_data)
plot_circular_cmase_by_condition <- function(cmase_data) {
  ggplot2::ggplot(cmase_data, ggplot2::aes(x = Condition, y = CMASE, fill = Selector)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65) +
    labs(
      title = expression(bold("Observed CMASE by Sensory Condition and Bandwidth Selector")),
      subtitle = "Mean circular prediction error per condition under each selector",
      y = expression(CMASE[obs]^"(z)"),
      x = "Sensory Condition",
      fill = "Selector"
    ) +
    scale_fill_manual(values = c("CV" = "gray50", "Bootstrap" = "gray30", "Rule-of-thumb" = "gray70")) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "top"
    )
}
