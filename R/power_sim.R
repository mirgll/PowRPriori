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
#'   comply with the names expected by the model. Correctly naming the variables is aided by the output of the `get_fixed_effects_structure()` helper function.
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
#' @param adjust_p_value Controls how p-values in the data simulation are adjusted when power is calculated for more than
#'  one parameter (as specified in `test_parameter`). Possible values are the same as in the function `p.adjust`. Defaults to "BH", which is the
#'  Benjamini-Hochberg correction. Setting this to `FALSE` disables p-value adjustment, although this is discouraged.
#' @param center Controls if the simulation should automatically center predictors. Defaults to
#'  `"auto"`, which extracts the centering attribute (if present) from the `fixed_effects` list. Set to `TRUE`
#'  to force mean-centering, or `FALSE` to disable it.
#' @param power_crit The desired statistical power level (e.g., 0.80 for 80%).
#' @param along A string specifying the sample size variable that the power analysis should be based on. Must be present in the
#'  `sample_size` variable of the `design` parameter.
#' @param n_start The starting sample size for the simulation. Defaults to `NULL`, in which case the number of the `along` parameter
#'  specified in the `design` parameter object is used.
#' @param n_increment The step size for increasing the sample size in each iteration.
#' @param max_simulation_steps A hard stop for the simulation, limiting the number of
#'   sample size steps to prevent infinite loops. Defaults to 100 steps.
#' @param n_issue_stop_prop The proportion of model issues (e.g., singular fits,
#'   non-convergence) at which the simulation will be automatically canceled. Defaults to a proportion of 20%.
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
#'     sample_size = list(subject = 20),
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
#'     center = TRUE,
#'     n_increment = 5,
#'     n_sims = 100, # Use low n_sims for quick examples
#'     parallel_plan = "multisession"
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
    adjust_p_value = "BH",
    center = "auto",
    power_crit = 0.80,
    along = NULL,
    n_start = NULL,
    n_increment,
    max_simulation_steps = 100,
    n_issue_stop_prop = 0.2,
    n_sims = 2000,
    alpha = 0.05,
    parallel_plan = "multisession") {

  # Temporarily enforce standard dummy coding to prevent issues with global user settings
  old_opts <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old_opts), add = TRUE)

  # 0. Basic input validation
  if (!is.numeric(power_crit) || power_crit <= 0) stop("`power_crit` must be a numeric value strictly greater than 0 and less than 1.", call. = FALSE)
  if (power_crit >= 1) {
    if (power_crit <= 100) stop(paste0("`power_crit` must be a proportion between 0 and 1. Did you mean ", power_crit / 100, " instead of ", power_crit, "?"), call. = FALSE)
    else stop("`power_crit` must be a numeric value between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(n_sims) || n_sims < 1 || n_sims %% 1 != 0) stop("`n_sims` must be a positive integer.", call. = FALSE)
  if (!is.numeric(n_increment) || n_increment < 1 || n_increment %% 1 != 0) stop("`n_increment` must be a positive integer.", call. = FALSE)
  if (!is.list(fixed_effects)) stop("`fixed_effects` must be a list. Tip: Use `get_fixed_effects_structure()`.", call. = FALSE)
  if (!is.null(random_effects) && !is.list(random_effects)) stop("`random_effects` must be a list. Tip: Use `get_random_effects_structure()`.", call. = FALSE)
  if (!family %in% c("gaussian", "binomial", "poisson")) stop(paste0("`family` must be one of: '", paste(c("gaussian", "binomial", "poisson"), collapse = "', '"), "'."), call. = FALSE)

  # 1. Error checks
  formula <- as.formula(formula)

  if (is.null(along)) {
    along <- names(design$sample_size)[length(design$sample_size)]
  } else if (!along %in% names(design$sample_size)) {
    stop(paste("The 'along' parameter must be one of the levels defined in sample_size:",
               paste(names(design$sample_size), collapse=", ")), call. = FALSE)
  }

  if(is.null(n_start)){
    n_start <- design$sample_size[[along]]
  }

  all_needed_vars <- all.vars(formula)[-1]
  all_defined_vars <- list()

  # Alle in sample_size definierten Variablen sammeln
  for (name in names(design$sample_size)) {
    all_defined_vars[[name]] <- NA
  }

  tmp_between_within_list <- list()
  if(!is.null(design$between) && !is.null(design$within)){
    tmp_between_within_list <- c(design$between, design$within)
  } else {
    if(!is.null(design$between)) tmp_between_within_list <- design$between
    else tmp_between_within_list <- design$within
  }

  if(!is.null(tmp_between_within_list)) {
    for(index in seq_along(tmp_between_within_list)) {
      level_name <- names(tmp_between_within_list)[index]
      element <- tmp_between_within_list[[index]]
      is_nested_level <- is.list(element) && !is.null(names(element)) && !all(c("mean", "sd") %in% names(element))

      if(is_nested_level) {
        all_defined_vars <- c(all_defined_vars, element)
        if(!level_name %in% names(all_defined_vars)) {
          all_defined_vars[[level_name]] <- NA
        }
      } else {
        all_defined_vars[[level_name]] <- element
      }
    }
  }

  all_defined_vars <- names(all_defined_vars)

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
      "Random effects were specified in `random_effects` or `icc_specs`, but the formula does not contain a random effects term.\n",
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

  # 2. Simulation Setup
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

  if (identical(center, "auto")) {
    is_centered_attr <- attr(fixed_effects, "is_centered")

    if (!is.null(is_centered_attr)) {
      center <- is_centered_attr
    } else{
      hasInteraction <- any(grepl(":", attr(stats::terms(nobars(formula)), "term.labels")))

      if(hasInteraction){
        stop("Your model contains interaction effects and you manually specified your fixed effects without specifying if the predictors should be centered. Since centering changes the interpretation and power of main effects, PowRPriori cannot safely guess your intent. Please explicitly add 'center = TRUE' (recommended if your weights are effect-coded) or 'center = FALSE' (if your weights use dummy-coding) to your power_sim() call.", call. = FALSE)
      } else {
        center <- FALSE
      }
    }
  }

  n_var <- along

  results_list <- list()
  current_n <- n_start
  last_power <- 0
  sim_step <- 0

  future::plan(parallel_plan)
  on.exit(future::plan("sequential"), add = TRUE)

  # Main loop for simulation and power analysis
  while (last_power < power_crit && sim_step < max_simulation_steps) {

    temp_design <- design
    temp_design$sample_size[[along]] <- current_n

    static_sizes <- temp_design$sample_size[names(temp_design$sample_size) != along]
    if (length(static_sizes) > 0) {
      sizes_str <- paste(paste(names(static_sizes), unlist(static_sizes), sep="="), collapse=", ")
      message(paste("\n--- Starting simulation for", along, "=", current_n, "| Static:", sizes_str, "|", n_sims, "simulations ---"))
    } else {
      message(paste("\n--- Starting simulation for", along, "=", current_n, "with", n_sims, "simulations ---"))
    }

    sim_results <- foreach(sim = 1:n_sims, .combine = "c", .multicombine = TRUE, .options.future = list(seed = TRUE)) %dofuture% {

      sim_data <- .create_design_matrix(temp_design, formula = formula)

      if (center) {
        sim_data <- .center_predictors(sim_data, formula, subject_id_col = ".PowR_id")
      }

      sim_data <- .simulate_outcome(sim_data, formula, fixed_effects, sds_random, family)

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
        p_val_col <- grep("Pr(>|.|)", names(model_summary), value=TRUE)
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

    p_values_ColNames <- names(diagnostics_df %>% dplyr::select(starts_with("p_")))

    if(!isFALSE(adjust_p_value) && length(p_values_ColNames) > 1) {
      diagnostics_df[p_values_ColNames] <- t(apply(diagnostics_df[p_values_ColNames],
                                                   MARGIN = 1,
                                                   FUN = stats::p.adjust,
                                                   method = adjust_p_value))
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
    model_issues_percent <- round((n_model_issues / n_sims) * 100)

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

      message(paste("Warning: Of", n_sims , "models calculated,", model_issues_percent , "% of fits had issues during simulation:",
                    paste(issue_details, collapse = ","), ".\nSimulation results are probably unreliable if a large proportion of model fits had issues."))
      if (has_critical_model_issues) {
        message(paste0("\n--- Simulation canceled ---\n",
                       "At N = ", current_n, ", ", round(n_model_issues / n_sims * 100), "% of models ",
                       "had fitting issues.\n",
                       "This could indicate a potential issue with the model specification or the defined parameters.\n",
                       "Please check model parameters (e.g. random effects structure, predictor scaling, etc.).\n"))
        break
      }
    }
    last_power <- if(length(power_values) > 0) min(power_values) else 0
    current_n <- current_n + n_increment
    sim_step <- sim_step + 1
  }

  temp_design <- design
  temp_design$sample_size[[along]] <- current_n
  final.sim.data <- .create_design_matrix(temp_design, formula = formula)

  if (center){
    final.sim.data <- .center_predictors(final.sim.data, formula, subject_id_col = ".PowR_id")
  }

  final.sim.data <- .simulate_outcome(final.sim.data, formula, fixed_effects, sds_random, family = family)

  final_results_df <- do.call(rbind, results_list)
  rownames(final_results_df) <- NULL
  names(final_results_df)[1] <- n_var
  if (!has_random_effects) final_results_df <- final_results_df %>% dplyr::select(!c("n_singular", "n_convergence", "n_other_warnings"))

  if(has_critical_model_issues) {
    message(paste0("\nSimulation stopped due to the percentage of simulated models showing fitting issues being larger than the specified `n_issue_stop_prop` parameter."))
  } else if (sim_step == max_simulation_steps) {
    message(paste0("\nSimulation stopped: Maximum number of simulation steps reached (", max_simulation_steps, "). If you did not change the `max_simulation_steps` parameter, this happened so the simulation does not run infintely.\nIf you want to continue with the current parameters, try increasing the `n_start` parameter."))
  } else {

    message(paste0("\nPower of at least ", power_crit, " achieved at N = ", final_results_df[[n_var]][nrow(final_results_df)], " (Power: ", round(last_power, 2), ")."))
    if(!isFALSE(adjust_p_value) && length(p_values_ColNames) > 1) {
      message("Note: Statistical power was calculated for more than one parameter. p-values were adjusted for multiple comparisons.")
    }
  }

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
    critical_model_issues = has_critical_model_issues,
    center = center
  )
  class(result) <- "PowRPriori"
  return(result)
}
