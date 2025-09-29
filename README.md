
# PowRPriori

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/mirgll/PSRtest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mirgll/PSRtest/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`PowRPriori` is an R package for conducting a priori power analyses for (generalized) linear mixed-effects models via data simulation. 
It provides a user-friendly and intuitive workflow for designing robust studies.

The core philosophy of `PowRPriori` is to provide researchers with a toolkit to conduct robust power analyses for their planned studies without having to 
take a deep dive into their statistical underpinnings. It provides useful helper functions aimed at making the process of properly setting up the simulation
as streamlined as possible. Additionally, it offers functions to run diagnostics on the plausibility of the simulated data and produce detailed summaries of the simulation outcome.

## Installation

You can install the development version of `PowRPriori` from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("mirgll/PowRPriori")
```
## Quick Start Example

Here is a minimal example of a power analysis for a 2x2 mixed design. We want to find the required sample size to detect an interaction effect with 80% power at an alpha level of .05.

```r
library(PowRPriori)
library(tidyr)

# 1. Define the study design
my_design <- define_design(
  id = "subject",
  between = list(group = c("Control", "Treatment")),
  within = list(time = c("pre", "post"))
)

# 2. Specify expected outcomes (means)
expected_means <- expand_grid(group = c("Control", "Treatment"), time = c("pre", "post"))
expected_means$mean_score <- c(50, 52, 50, 60) # Control: 50->52, Treatment: 50->60

# 3. Define the statistical model and derive parameters
my_formula <- score ~ group * time + (1|subject)
my_fixed_effects <- fixed_effects_from_average_outcome(my_formula, expected_means)
my_random_effects <- list(subject = list(`(Intercept)` = 8), sd_resid = 12)

# 4. Run the simulation
# (n_sims should be >= 1000 for a real analysis)
power_results <- power_sim(
  formula = my_formula,
  design = my_design,
  fixed_effects = my_fixed_effects,
  random_effects = my_random_effects,
  test_parameter = "groupTreatment:timepost",
  n_start = 30,
  n_increment = 10,
  n_sims = 100 # Low number for a quick example
)

# 5. Plot the results and view summary
plot_sim_model(power_results)
summary(power_results)
```

## Learn More

For a detailed walkthrough of all features, please see the package vignette:

```r
vignette("Workflow-Example", package = "PowRPriori")
```
