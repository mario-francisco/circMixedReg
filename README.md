---
title: "circMixedReg: Nonparametric Circular Regression with Mixed Predictors"
output:
  html_document: default
  pdf_document: default
---

# circMixedReg

[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.1-1f425f.svg)](https://cran.r-project.org/)

**Nonparametric Circular Regression with Mixed-Type Predictors**

---

## Overview

**`circMixedReg`** provides flexible tools for nonparametric regression when the response variable is **circular** (e.g., angles, directions) and the predictors are a **combination of continuous and categorical variables**.  
The package implements **Nadaraya–Watson** and **Local Linear** estimators, multiple **bandwidth selection methods**, graphical tools for **diagnostics**, **residual analysis**, and **confidence bands**.

Additionally, it includes a real-world dataset from a spatial updating experiment with varying sensory conditions.

---

## Installation

You can install the development version from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("mario-francisco/circMixedReg")
```

---

## Citation and version (for reproducibility)

The results in the accompanying manuscript were obtained with **circMixedReg v0.1.0** (GitHub Release **v0.1.0**).

To install exactly that version:

```r
devtools::install_github("mario-francisco/circMixedReg", ref = "v0.1.0")

---

## Quick Example

```r
library(circMixedReg)

# Simulate synthetic circular regression data
set.seed(123)
data <- simulate_circular_data_gen(n = 100,
                                   mu_fun = function(x, z) (2 * pi * x) %% (2 * pi),
                                   kappa = 5)

# Fit a Nadaraya–Watson regression
fit <- sapply(seq(0, 1, length.out = 100), function(x0) {
  nw_circular_mixed(x0 = x0, z0 = data$Z[1],
                    X = matrix(data$X, ncol = 1),
                    Z = data.frame(Z = data$Z),
                    theta = data$theta,
                    h = 0.3, lambda = 0.5)
})

# Plot the estimated regression curve
plot(seq(0, 1, length.out = 100), fit, type = "l",
     main = "Circular Regression Curve", xlab = "X", ylab = "Predicted Angle (radians)")
```

---

## Included Dataset

The package includes the **`legge_data`** dataset, derived from the spatial updating experiments conducted by Legge et al. (2016).  
It provides real-world trial-level data on human navigation under sensory restrictions, including angular errors, distances, and sensory conditions.

Example usage:

```r
# Load the included dataset
data(legge_data)

# Inspect the data
summary(legge_data)
head(legge_data)
```

Reference:
> Legge, G. E., Granquist, C., Baek, Y., & Gage, R. (2016).  
> *Indoor spatial updating with impaired vision*.  
> Investigative Ophthalmology & Visual Science, 57(15), 6757–6765.  
> https://doi.org/10.1167/iovs.16-20226

---

## Features

- **Circular response** modeling with mixed-type predictors (continuous + categorical).
- **Nadaraya–Watson** and **Local Linear** regression estimators.
- **Cross-validation**, **bootstrap**, and **rule-of-thumb** bandwidth selectors.
- **Simultaneous bootstrap confidence bands** with arrows or ribbons.
- **Model diagnostics** and **residual analysis** adapted to circular data.
- **Synthetic data simulation** and **real-world dataset** included.

---

## License

This package is distributed under the **GPL-3 License**.

---

## Contact

Maintainer: **Mario Francisco-Fernandez**  
Email: `mario.francisco@udc.es`

---

