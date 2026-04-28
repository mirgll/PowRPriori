## Test environments
* Local Windows 11 (Build 23H2), R 4.5.2
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Update
This is an update to an existing package, in which new features have been implemented, existing functions have been refactored / changed, some bugs were fixed and the vignette has been updated.

1. New features:
  - The package now supports mean centering for predictors and automated p-value adjustment for multiple testing
  - A new internal helper function `center_predictors` has been implemented which handles predictor mean centering
2. Existing function changes:
  - New parameters have been added to the `power_sim`, `fixed_effects_from_average_outcomes`, and `plot_sim_model` to accommodate the new mean centering and p-value adjustment functionality
  - The `define_design` function has been changed to offer a more intuitive, streamlined definition of research designs. The old `id` parameter has been replaced by a new `sample_size` parameter, where all analysis levels of the design are explicitly specified and correctly handled internally
  - The internal `.create_design_matrix` function has been changed to correctly handle the new objects created by updated `define_design` function
3. Several bug fixes, including an important bug where the `plot_sim_model` function did not calculate the correct average outcome value when used with `type="data"`
4. Updated the vignette to provide tutorials for the new features and reflect the changes to the mentioned function parameters   
   
## Reverse dependencies
There are currently no reverse dependencies for this package.
