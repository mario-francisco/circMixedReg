% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_circular_residuals.R
\name{analyze_circular_residuals}
\alias{analyze_circular_residuals}
\title{Analyze Residuals in Circular Regression}
\usage{
analyze_circular_residuals(
  data,
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
  units = "radians"
)
}
\arguments{
\item{data}{Data frame with the sample.}

\item{response_col}{Name of the circular response column (in radians).}

\item{cont_cols}{Character vector of continuous predictor names.}

\item{cat_cols}{Character vector of categorical predictor names.}

\item{h}{Bandwidths for continuous variables.}

\item{lambda}{Smoothing parameters for categorical variables.}

\item{kernel_continuous}{Kernel function for continuous variables.}

\item{kernel_categorical}{Kernel function for categorical variables.}

\item{kernel_levels}{Optional list of levels (for ordinal kernels).}

\item{estimator}{Type of estimator: "NW" (Nadaraya–Watson) or "LL" (local linear). Default is "NW".}

\item{group_col}{Optional: name of column for grouping residuals by category.}

\item{bins}{Number of bins in the circular histogram (for global and group histograms).}

\item{circular_boxplot}{Logical. If TRUE, shows circular boxplots by group using bpDir.}

\item{violin_plot}{Logical. If TRUE, shows violin plots by group using ggplot2.}

\item{circular_boxplot_file}{Optional. If not NULL, saves circular boxplots to this PDF file.}

\item{violin_plot_file}{Optional. If not NULL, saves violin plot to this PDF file.}

\item{histogram_by_group}{Logical. If TRUE, shows circular histograms of residuals by group.}

\item{histogram_file}{Optional. If not NULL, saves histograms by group to this PDF file.}

\item{place}{Label placement for circular boxplot ("outside", "inside", "none").}

\item{units}{Units for circular boxplot labels ("radians" or "degrees").}
}
\value{
A list with residuals, fitted values, CMASE_obs, R²_circular and uniformity test results.
}
\description{
Computes and visualizes diagnostic tools for circular regression residuals,
including CMASE_obs, circular R², histogram, residuals vs. fitted plot, and uniformity tests.
It also supports grouped visualizations (by category) using circular histograms,
violin plots, and circular boxplots, optionally saving them to PDF files.
}
\examples{
# See examples in compute_residuals and predict_circular_mixed
}
