#' Perform a Power Analysis for (Generalized) Linear Mixed-Effects Models via Data Simulation
#'
#' @description
#' This is the main function of the `PowRPriori` package. It iteratively simulates
#' datasets for increasing sample sizes to determine the required sample size
#' to achieve a desired level of statistical power for specific model parameters.
#'
#' @details
#' The function supports parallel computation using `future`.
#' Simple linear models (i.e. regression models) can also be analyzed using this function. In this case, no specification of the `random_effects` or `icc_specs`
#' parameter is necessary.`icc_specs` should only be used when simulating a model containing only random intercepts and no random slopes. Refer to the
#' vignette for a more detailed description of the complete workflow for using this function.
#'
#'
#' @param formula An lme4-style model formula (e.g. `outcome ~ predictor1 * predictor2 + (1 | id)`).
#' @param design A `PowRPriori_design` object created by `define_design()`.
#' @param test_parameter A character vector of the variable names to test for power.
#'   If `NULL` (default), power is calculated for all fixed effects except the intercept. Note: The parameter names need to
#'   comply with the names expected by the model. Correctly naming of the variables is aided by the output of the `get_fixed_effects_structure()` helper function.
#' @param fixed_effects A named list of the fixed-effects coefficients. It is highly
#'   recommended to generate this using `get_fixed_effects_structure()` or
#'   `fixed_effects_from_average_outcome()`.
#' @param random_effects A named, nested list specifying the standard deviations (SDs)
#'   and (if applicable) correlations of the random effects. It is highly
#'   recommended to generate this using `get_random_effects_structure()`. If this parameter is not used, `icc_specs` and `overall_variance` need to be supplied.
#' @param icc_specs Optional. A named list of Intraclass Correlation Coefficients for
#'   defining simple random-intercept models. Must be used with `overall_variance`.
#' @param overall_variance The total variance of the outcome, required when `icc_specs` is used.
#' @param family The model family: `"gaussian"` (for LMMs), `"binomial"` (for logistic GLMMs),
#'   or `"poisson"` (for poisson GLMMs).
#' @param power_crit The desired statistical power level (e.g., 0.80 for 80%).
#' @param n_start The starting sample size for the simulation.
#' @param n_increment The step size for increasing the sample size in each iteration.
#' @param max_simulation_steps A hard stop for the simulation, limiting the number of
#'   sample size steps to prevent infinite loops. Defaults to 40 steps.
#' @param n_issue_stop_prop The proportion of model issues (e.g., singular fits,
#'   non-convergence) at which the simulation will be automatically canceled. Defaults to a proportion of 20%.
#' @param n_is_total Boolean that controls how sample sizes are interpreted. If `TRUE`
#'   (default), `n_start` refers to the total sample size. If `FALSE`, it refers to
#'   the sample size per cell (see `define_design()` for details on nested designs).
#' @param n_sims The number of simulations to run for each sample size step. Defaults to 2000.
#' @param alpha The significance level (alpha) for the power calculation. Defaults to 0.05.
#' @param parallel_plan A string specifying the `future` plan for parallel processing.
#'   Defaults to `"multisession"` to enable parallel computing. Use `"sequential"` for debugging.
#'
#' @return An object of class `PowRPriori`, which is a list containing the power table,
#'   a sample dataset, all simulation parameters, and detailed results from all runs
#'   (coefficients and random effect estimates).
#'
#' @importFrom foreach foreach
#' @importFrom doFuture `%dofuture%`
#' @importFrom lmerTest lmer
#' @importFrom lme4 isSingular glmer glmerControl lmerControl
#' @importFrom future plan
#' @importFrom dplyr starts_with
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#'
#'   design <- define_design(
#'     id = "subject",
#'     between = list(group = c("Control", "Treatment")),
#'     within = list(time = c("pre", "post"))
#'   )
#'
#'   fixed_effects <- list(
#'     `(Intercept)` = 10,
#'     groupTreatment = 2,
#'     timepost = 1,
#'     `groupTreatment:timepost` = 1.5
#'   )
#'
#'   random_effects <- list(
#'     subject = list(`(Intercept)` = 3),
#'     sd_resid = 5
#'   )
#'\donttest{
#'   power_results <- power_sim(
#'     formula = y ~ group * time + (1|subject),
#'     design = design,
#'     fixed_effects = fixed_effects,
#'     random_effects = random_effects,
#'     test_parameter = "groupTreatment:timepost",
#'     n_start = 20,
#'     n_increment = 5,
#'     n_sims = 100 # Use low n_sims for quick examples
#'   )
#'
#'   summary(power_results)
#'   plot_sim_model(power_results)
#'}

power_sim <- function(
    formula,
    design,
    test_parameter = NULL,
    fixed_effects,
    random_effects = NULL,
    icc_specs = NULL,
    overall_variance = NULL,
    family = "gaussian",
    power_crit = 0.80,
    n_start,
    n_increment,
    max_simulation_steps = 40,
    n_issue_stop_prop = 0.2,
    n_is_total = TRUE,
    n_sims = 2000,
    alpha = 0.05,
    parallel_plan = "multisession") {
# 1. Error checks
  formula <- as.formula(formula)
  all_needed_vars <- all.vars(formula)[-1]
  all_defined_vars <- c(design$id, names(design$between), names(design$within))

  if (n_issue_stop_prop < 0 || n_issue_stop_prop > 1) {
    stop("'n_issue_stop_prop' needs to be a proportion, i.e. have a value between 0 and 1.", call. = FALSE)
  }

  if (is.null(test_parameter)) {
    test_parameter <- grep("(Intercept)", names(get_fixed_effects_structure(formula, design)),
                           value = TRUE, invert = TRUE, fixed = TRUE)
    message(paste0(
      "Info: `test_parameter` value left at NULL. Power will be calculated for all fixed effects:\n -> ",
      paste(test_parameter, collapse = ", ")
    ))
  }

  missing_vars <- setdiff(all_needed_vars, all_defined_vars)
  has_random_effects <- length(lme4::findbars(formula)) > 0

  if (length(missing_vars) > 0) {
    stop(
      "The following variables present in the formula were not found in the design object: ",
      paste(missing_vars, collapse = ", "), "\nYou may want to check for spelling errors or misspecified variables in either the formula or the design object.",
      call. = FALSE
    )
  }

  if (has_random_effects && is.null(random_effects) && is.null(icc_specs)) {
    stop(
      "Random effects were specified (in `random_effects` or `icc_specs`), but the formula does not contain a random effects term.\n",
      call. = FALSE
    )
  }

  all_fixed_effects <- names(get_fixed_effects_structure(formula, design))
  mismatched_test_parameter <- setdiff(test_parameter, all_fixed_effects)

  if (length(mismatched_test_parameter) > 0) {
    stop(
      "The following parameter(s) specified in 'test_parameter' do not exist in the model: ",
      paste(mismatched_test_parameter, collapse = ", "),
      "\n  -> Usable parameters to be tested are: ", paste(all_fixed_effects, collapse = ", "),
      call. = FALSE
    )
  }

  missing_fe_names <- setdiff(all_fixed_effects, names(fixed_effects))
  extra_fe_names <- setdiff(names(fixed_effects), all_fixed_effects)

  if(length(missing_fe_names) > 0) {
    stop("The fixed effects parameter list is incomplete. The following fixed effects are missing:\n",
         paste(missing_fe_names, collapse = ", "), call. = FALSE)
  }

  if(length(extra_fe_names) > 0) {
    stop("The fixed effects parameter list contains effects not specified in the formula:\n",
         paste(extra_fe_names, collapse = ", "), call. = FALSE)
  }

  if (!is.null(random_effects) & has_random_effects) {
    expected_re_names <- get_random_effects_structure(formula, design, family = family)
    missing_re_groups <- setdiff(names(expected_re_names), names(random_effects))
    extra_re_groups <- setdiff(names(random_effects), names(expected_re_names))

    if (length(missing_re_groups) > 0){
      stop("The random effects parameter list is incomplete. The following random effects are missing:\n",
           paste(missing_re_groups, collapse = ", "), call. = FALSE)
    }
    if (length(extra_re_groups) > 0)  {
      stop("The random effects parameter list contains effects not specified in the formula:\n",
           paste(extra_re_groups, collapse = ", "), call. = FALSE)
    }
    for (group in names(expected_re_names)) {
      if (is.list(expected_re_names[[group]])) {
        expected_names <- names(expected_re_names[[group]])
        provided_names <- names(random_effects[[group]])
        if (!all(expected_names %in% provided_names)) {
          stop("Name error in `random_effects` parameter for effect: '", group, "'. Expected names are: ",
               paste(expected_names, collapse = ", "), call. = FALSE)
        }
      }
    }
  }

  if(is.null(random_effects) & is.null(icc_specs) & has_random_effects){
    stop("The supplied formula contains random effects.\nYou need to specify either a random effects structure or ICCs for your design.", call. = FALSE)
  }
# 2. Simulation
  if (has_random_effects) {
    if (!is.null(random_effects)) {
      if (family == "gaussian" && is.null(random_effects$sd_resid)) {
        stop("The 'random_effects' list must contain 'sd_resid' for gaussian models.", call. = FALSE)
      }
      sds_random <- random_effects
    } else if (!is.null(icc_specs)) {
      expected_re <- get_random_effects_structure(formula, design)
      for (group in names(icc_specs)) {
        if (length(expected_re[[group]]) > 1) {
          stop(paste0(
            "Random slopes specified for factor: '", group, "'.\n",
            "Definition of random effects via ICCs is only possible for random intercept models.\n",
            "Please use 'random_effects' parameter to properly define all random effects."
          ), call. = FALSE)
        }
      }

      if (is.null(overall_variance)) {
        stop("When using 'icc_specs', 'overall_variance' must also be supplied.", call. = FALSE)
      }
      sds_random <- list()
      re_variances <- lapply(icc_specs, function(icc) icc * overall_variance)
      var_resid <- overall_variance - sum(unlist(re_variances))
      if (var_resid < 0) {
        stop("Negative residual variance. Did you specify a sum of ICCs > 1?", call. = FALSE)
      }
      for (group_name in names(re_variances)) {
        sds_random[[group_name]] <- list(
          `(Intercept)` = sqrt(re_variances[[group_name]])
        )
      }
      sds_random$sd_resid <- sqrt(var_resid)

    } else stop("The formula contains random effects, but neither `random_effects` nor `icc_specs` were specified.", call. = FALSE)
  } else {
    if (family == "gaussian") {
      if (is.null(random_effects$sd_resid)) {
        stop("For a linear model (family='gaussian' without random effects), `random_effects` must be a list containing 'sd_resid' (the residual variance).", call. = FALSE)
      }
      sds_random <- random_effects
    } else {
      sds_random <- list(sd_resid = 0)
    }
  }

  n_var <- design$id
  between_vars <- names(design$between)

  data_structure <- c(
    setNames(list(NA), n_var),
    design$between,
    design$within
  )

  results_list <- list()
  current_n <- n_start
  last_power <- 0
  sim_step <- 1

  future::plan(parallel_plan)

  # Main loop for simulation and power analysis
  while (last_power < power_crit && sim_step <= max_simulation_steps) {
    message(paste("\n--- Starting simulation for", n_var, "=", current_n, "with", n_sims, "simulations ---"))

    sim_results <- foreach(sim = 1:n_sims, .combine = "c", .multicombine = TRUE, .options.future = list(seed = TRUE)) %dofuture% {
        sim_data <- .create_design_matrix(design, current_n, n_is_total) %>%
          .simulate_outcome(., formula, fixed_effects, sds_random, family)

      model_fit <- NULL
      has_conv_warning <- FALSE
      has_error <- FALSE
      error_msg <- NA_character_
      has_identifiable_warning <- FALSE
      has_other_warning <- FALSE

      suppressMessages(suppressWarnings(
        tryCatch(
          {
            if (has_random_effects) {
              if (family == "gaussian") {
                model_fit <- lmerTest::lmer(formula, data = sim_data, control = lme4::lmerControl(optimizer = "bobyqa"))
              } else {
                model_fit <- lme4::glmer(formula, data = sim_data, family = family,  control = lme4::glmerControl(optimizer = "bobyqa"))
              }
            } else {
              if (family == "gaussian") {
                model_fit <- stats::lm(formula, data = sim_data)
              } else {
                model_fit <- stats::glm(formula, data = sim_data, family = family)
              }
            }
          },
          warning = function(w) {
            if (grepl("failed to converge|degenerate", w$message, ignore.case = TRUE)) {
              has_conv_warning <<- TRUE
            } else if (grepl("nearly unidentifiable|very large eigenvalue", w$message, ignore.case = TRUE)) {
              has_identifiable_warning <<- TRUE
            } else {
              has_other_warning <<- TRUE
            }
          },
          error = function(e) {
            has_error <<- TRUE
            error_msg <<- e$message
          }
        )
      ))

      p_values <- setNames(rep(NA_real_, length(test_parameter)), paste0("p_", test_parameter))
      singular <- FALSE
      coeffs <- NA
      re_estimates <- NA

      if(!is.null(model_fit)) {
        model_summary <- as.data.frame(summary(model_fit)$coefficients)
        p_val_col <- grep("Pr(>|.|)", names(model_summary), value=T)
        for(param in test_parameter) {
          if (param %in% rownames(model_summary)) {
            p_values[paste0("p_", param)] <- model_summary[param, p_val_col]
          }
        }
        coeffs <- stats::coef(model_fit)

        if (inherits(model_fit, "merMod")) {
          singular <- lme4::isSingular(model_fit)
          if (singular || has_conv_warning || has_identifiable_warning) {
            p_values[] <- 1.0
          }

          vc <- lme4::VarCorr(model_fit)
          re_list <- list()
          for(group in names(vc)){
            sds <- attr(vc[[group]], "stddev")
            names(sds) <- paste0(group, ".SD.", names(sds))
            re_list <- c(re_list, as.list(sds))
            cors <- attr(vc[[group]], "correlation")
            if(nrow(cors) > 1){
              values_upper_tri <- cors[upper.tri(cors)]
              name_matrix <- outer(
                rownames(cors), colnames(cors),
                function(r, c) paste0(group, ".cor_", r, "__", c)
              )
              names_upper_tri <- name_matrix[upper.tri(name_matrix)]
              names(values_upper_tri) <- names_upper_tri
              re_list <- c(re_list, as.list(values_upper_tri))
            }
          }
          if(family == "gaussian") re_list$sd_resid <- attr(vc, "sc")
          re_estimates <- re_list

        } else {
            coeffs <- stats::coef(model_fit)
            if(family == "gaussian") re_estimates <- list(sd_resid = summary(model_fit)$sigma)
        }
      }

      for(group in names(coeffs)){
        names(coeffs[[group]]) <- gsub("[()]", "", names(coeffs[[group]]))
      }
      names(re_estimates) <- gsub("[()]", "", names(re_estimates))

      list(
        diagnostics = data.frame(as.list(p_values), singular = singular,
                                 conv_warning = has_conv_warning, other_warning = has_other_warning,
                                 identifiable_warning = has_identifiable_warning,
                                 error = has_error, error_msg = error_msg),
        coefficients = if(is.list(coeffs)) coeffs[[1]] else if(is.numeric(coeffs)) as.data.frame(t(coeffs)) else NA,
        re_estimates = if(is.list(re_estimates)) as.data.frame(re_estimates, check.names = FALSE) else NA
      )
    }

    diagnostics_df <- do.call(rbind, sim_results[names(sim_results) == "diagnostics"])
    coefficients_df <- do.call(rbind, sim_results[names(sim_results) == "coefficients"])
    re_estimates_df <- do.call(rbind, sim_results[names(sim_results) == "re_estimates"])

    n_errors <- sum(diagnostics_df$error, na.rm = TRUE)

    if (n_errors > 0) {
      stop(paste0(
        "Simulation stopped: ", n_errors, " out of ", n_sims, " model fits failed with an error.\n\n",
        "This indicates a fundamental problem with the specified model or parameters.\n",
        "The first captured error was:\n  -> '", stats::na.omit(diagnostics_df$error_msg)[1], "'"
      ), call. = FALSE)
    }

    power_values <- colMeans(diagnostics_df %>% dplyr::select(starts_with("p_")) < alpha, na.rm = TRUE)
    names(power_values) <- sub("^p_", "power_", names(power_values))

    power_ci <- lapply(power_values, function(p) {
      se <- sqrt(p * (1 - p) / n_sims)
      lower <- round(p - 1.96 * se, 3)
      upper <- round(p + 1.96 * se, 3)
      return(data.frame(lower = lower, upper = upper))
    })

    ci_df <- as.data.frame(do.call(cbind, power_ci))
    names(ci_df) <- paste0(rep(c("ci_lower_", "ci_upper_"), times=length(power_values)),
                           sub("(\\.lower|\\.upper)", "", sub("power_", "", names(ci_df))))

    n_singular <- sum(diagnostics_df$singular, na.rm = TRUE)
    n_convergence <- sum(diagnostics_df$conv_warning, na.rm = TRUE)
    n_identifiable <- sum(diagnostics_df$identifiable_warning, na.rm = TRUE)
    n_other_warnings <- sum(diagnostics_df$other_warning, na.rm = TRUE)

    n_model_issues <- n_singular + n_convergence + n_identifiable
    has_critical_model_issues <- n_model_issues > (n_sims * n_issue_stop_prop)

    results_list[[as.character(current_n)]] <- data.frame(
      n = current_n,
      as.list(power_values),
      ci_df,
      n_singular = n_singular,
      n_convergence = n_convergence,
      n_identifiable = n_identifiable,
      n_other_warnings = n_other_warnings
    )

    power_summary_string <- paste(
      paste0(sub("power_", "", names(power_values)), ": ", round(power_values, 2)),
      collapse = " | "
    )

    if(has_random_effects){
      message(paste("Power (", power_summary_string, ") | Model Issues:", n_model_issues,
                  "| Other Warnings:", n_other_warnings, "\n---"))
    } else {
      message(paste("Power (", power_summary_string, ")"))
    }

    if(has_random_effects & n_model_issues > 0){
      issue_details <- c()
      if (n_singular > 0) issue_details <- c(issue_details, paste0(" Singular Fits: ", n_singular))
      if (n_convergence > 0) issue_details <- c(issue_details, paste0(" Non-Convergence: ", n_convergence))
      if (n_identifiable > 0) issue_details <- c(issue_details, paste0(" Nearly Unidentifiable Fits: ", n_identifiable))

      message(paste("Warning: Some model fits had issues during simulation:",
                    paste(issue_details, collapse = ","), ".\nSimulation results might be unreliable if a large proportion of model fits had issues."))
      if (has_critical_model_issues) {
        message(paste0("\n--- Simulation canceled ---\n",
                       "At N = ", current_n, ", ", round(n_model_issues / n_sims * 100), "% of models ",
                       "had fitting issues.\n",
                       "This suggests a potential issue with the model specification or the defined parameters.\n",
                       "Please check model parameters (e.g. random effects structure, predictor scaling, etc.)."))
        break
      }
    }
    last_power <- if(length(power_values) > 0) min(power_values) else 0
    current_n <- current_n + n_increment
    sim_step <- sim_step + 1
  }
  final.sim.data <- .create_design_matrix(design, current_n, n_is_total) %>%
                    .simulate_outcome(formula, fixed_effects, sds_random, family = family)
  final_results_df <- do.call(rbind, results_list)
  rownames(final_results_df) <- NULL
  names(final_results_df)[1] <- n_var
  if (!has_random_effects) final_results_df <- final_results_df %>% dplyr::select(!c("n_singular", "n_convergence", "n_other_warnings"))
  if(!has_critical_model_issues) message(paste0("\nPower of at least ", power_crit, " achieved at N = ", final_results_df[[n_var]][nrow(final_results_df)], " (Power: ", round(last_power, 2), ")."))

  names(coefficients_df) <- gsub("[()]", "", names(coefficients_df))
  names(fixed_effects) <- gsub("[()]", "", names(fixed_effects))
  if(!is.null(random_effects)) {
    for(group in names(random_effects)){
      if(group != "sd_resid") names(random_effects[[group]]) <- gsub("[()]", "", names(random_effects[[group]]))
    }
  }

  result <- list(
    power_table = final_results_df,
    sample_data = final.sim.data,
    formula = formula,
    design = design,
    family = family,
    parameters = list(
      fixed_effects = fixed_effects,
      random_spec = if(!is.null(random_effects)) random_effects else list(icc_specs = icc_specs, overall_variance = overall_variance)
    ),
    power_crit = power_crit,
    all_coefficients = as.data.frame(coefficients_df),
    all_re_estimates = as.data.frame(re_estimates_df, check.names = FALSE),
    critical_model_issues = has_critical_model_issues
  )
  class(result) <- "PowRPriori"
  return(result)
}
