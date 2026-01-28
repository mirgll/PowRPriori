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
