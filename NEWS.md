# PowRPriori 0.2.0

This is a large update, including complete refactoring of underlying mechanisms related to how the package handles experimental designs and the addition of mean centering as well as p-value adjustment. 
The way study designs are defined has been changed in a major way, which also led to changes of other functions in the package to work correctly with the new functionality.
As a result, the process of doing power analyses should now be more streamlined and intuitive as well as support a larger variety of research designs. 
The vignette has also been adapted to reflect these changes. 

## Breaking Changes
* The `id` parameter in `define_design` has been removed. Grouping variables and hierarchical structures are now defined entirely by a new `sample_size` parameter, providing a much cleaner and more intuitive syntax
* The `between` parameter in `define_design()` has been changed. Now, the predictor can directly reference the  desired analysis unit defined in `sample_size`
* Some functions were changed in order to support the new mean centering functionality:
  * `fixed_effects_from_average_outcome` now has a new function parameter `center`. It defaults to `TRUE` and controls how the centering of predictors is handled. See function documentation for more details
  * The `n_is_total` in `power_sim` has been removed. This choice has been made obsolete by the new `sample_size` parameter in `define_design`
  * The `plot_sim_model` function now also has a `center` parameter that works identically to the one added in `power_sim`
* In cases where users supply the fixed effects manually to `power_sim` (i.e. do not use `fixed_effects_from_average_outcome`) and an interaction effect is present in the formula, `power_sim` now stops execution and alerts the user to explicitly choose if centering should be applied

## New Features & Parameters
* Added automated p-value adjustment (Benjamini-Hochberg by default) in `power_sim` when testing multiple effects for power simultaneously. See function documentation for details
* Added a mean centering functionality for predictors. Users can now configure if the predictors should be centered in the `power_sim` function
  `PowRPriori` automatically chooses either grand-mean centering or within-cluster centering (between-subject predictors are centered on the grand mean, within-subject predictors are centered on the cluster mean). See function documentation and vignette for details
* `power_sim` has new parameters: `adjust_p_value`, `along` and `center` 
  * `adjust_p_value` controls the method of p-value adjustment when multiple parameters are specified in `test_parameter`
  * `along` defines which variable specified in the `sample_size` parameter in `define_design` should be incremented after each simulation course (defaults to `NULL`, in which case the function increments the lowest analysis unit)
  * `center` controls if mean centering should be applied to the predictors. Defaults to `auto`, in which case the simulation engine tries to detect the appropriate centering method
* Improved the underlying data simulation engine (updated `.create_design_matrix` and implemented a new function `.center_predictors`) to automatically and robustly handle complex hierarchical and crossed designs, as well as automatic mean-centering
* Added a new parameter `n` to `plot_sim_model`, which allows the configuration of the sample size to be simulated for the plot and defaults to the sample size of the lowest analysis level otherwise
* Added a new internal function `.center_predictors` which handles the predictor mean centering

## General improvements / changes
* The internal `.create_design_matrix` was updated to correctly handle the new objects created by `define_design`
* Updated the vignette to reflect and incorporate all changes and new features

## Bug fixes
* Fixed an issue where `plot_sim_model` calculated the wrong average sample value for the data when using `type="data"`

# PowRPriori 0.1.2

This a smaller update, improving the vignette and fixing a critical bug in the functions providing code snippets for model specification as well as some smaller bugs.

## Bug fixes

### Critical bug fix
* Fixed incorrect handling of certain nested designs in `get_fixed_effects_structrue` and `get_random_effects_structure` so that the functions now consistently produce the correct code snippets

### Miscellaneous smaller bugs
* Fixed plotting of sample data where the incorrect regression lines were drawn when using `plot_sim_model` with `type = "data"`
* Corrected the possible values of the `type` parameter in the `plot_sim_model` function when using it with an `lme4`-style formula object
* Fixed incorrect console output when the data simulation with `power_sim` reached or exceeded `max_simulation_steps`
* Fixed a bug where the simulation stop condition did not work correctly in some cases where more than the defined `n_issue_stop_prop` proportion of models had fitting issues

## General improvements

* Vastly extended the vignette to provide more background information on the functions, include more additional use-cases and further improve the overall readability
* Improved the output of `power_sim` to include more information when simulating a nested design
* Improved the output of `power_sim` to incorporate the percentage of model fits with issues in relation to the total number of models fit

# PowRPriori 0.1.1

* Initial CRAN submission
