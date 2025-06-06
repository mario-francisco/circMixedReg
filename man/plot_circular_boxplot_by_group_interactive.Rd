% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_circular_boxplot_by_group_interactive.R
\name{plot_circular_boxplot_by_group_interactive}
\alias{plot_circular_boxplot_by_group_interactive}
\title{Plot Circular Boxplots of Residuals by Group (Interactive)}
\usage{
plot_circular_boxplot_by_group_interactive(
  resids,
  groups,
  place = "outside",
  units = "radians"
)
}
\arguments{
\item{resids}{Vector of residuals (in radians).}

\item{groups}{Grouping variable (factor or character).}

\item{place}{Position of the angle labels: "outside", "inside" or "none".}

\item{units}{Units for the angle labels: "radians" (default) or "degrees".}
}
\value{
No return value; generates plots as side effects.
}
\description{
Creates one circular boxplot per group and displays them interactively
using the \code{bpDir::CircularBoxplot} function.
}
\examples{
\dontrun{
  # Simulate example
  set.seed(1)
  resids <- runif(100, -pi, pi)
  groups <- sample(c("A", "B", "C"), 100, replace = TRUE)
  plot_circular_boxplot_by_group_interactive(resids, groups)
}

}
