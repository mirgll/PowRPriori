#' @importFrom stats as.formula sd setNames
NULL


#' Safely Convert Character Vectors to Factors
#'
#' @description
#' An internal helper function that converts a character vector to a factor,
#' ensuring the level order is based on the first appearance of each element.
#' If the input is not a character vector, it's returned unchanged.
#'
#' @param x A vector from a design specification.
#' @return A factor with levels in order of appearance, or the original object.
#test
.to_factor_safely <- function(x) {
  if (is.character(x)) {
    return(factor(x, levels = unique(x)))
  }

  return(x)
}

#' Internal Helper Function for Mean-Centering
#'
#' @description
#' This helper function is responsible for the mean centering of predictors. It detects whether any given predictor
#' is repeated within clusters (e.g. repeated measurements in participants) and automatically applies within-cluster
#' mean centering if this is the case. In all other cases, grand mean centering is applied.
#'
#' @param df The dataframe to process
#' @param formula The lme4 formula to extract predictors
#' @param subject_id_col The name of the subject identifier column
#'
#' @return Dataframe with centered predictors

.center_predictors <- function(df, formula, subject_id_col = ".PowR_id") {
  predictors <- all.vars(lme4::nobars(formula))[-1]
  has_subject <- subject_id_col %in% names(df)

  for (v in predictors) {
    if (v %in% names(df)) {
      if (is.numeric(df[[v]])) {
        if (has_subject) {
          is_constant <- all(tapply(df[[v]], df[[subject_id_col]], function(x) length(unique(x)) == 1))
        } else {
          is_constant <- TRUE
        }
        if (is_constant) {
          df[[v]] <- df[[v]] - mean(df[[v]], na.rm = TRUE)
        } else {
          person_means <- stats::ave(df[[v]], df[[subject_id_col]], FUN = function(x) mean(x, na.rm = TRUE))
          df[[v]] <- df[[v]] - person_means
        }
      } else if (is.factor(df[[v]]) || is.character(df[[v]])) {
        df[[v]] <- as.factor(df[[v]])
        n_levels <- nlevels(df[[v]])
        contr <- stats::contr.treatment(n_levels)
        colnames(contr) <- levels(df[[v]])[-1]

        props <- prop.table(table(df[[v]]))
        for (col in colnames(contr)) {
          contr[, col] <- contr[, col] - props[col]
        }
        stats::contrasts(df[[v]]) <- contr
      }
    }
  }
  return(df)
}

#' Internal helper for checking multilevel predictor assignment
#'
#' @param var_list A between or within variable list from a `define_design` function call
#' @param arg_name A string to inform whether a between or within variable has been provided in var_list
#' @param level_names A character vector of the multilevel variables
#' @param is_multilevel A boolean signifying whether the design has hierarchical dependencies
#'
#' @returns A list of the variables specified in a `define design` function call, wrapped in a named list corresponding to the specified levels in the function call
#' @noRd
.check_and_wrap <- function(var_list, arg_name, level_names, is_multilevel) {
  if (is.null(var_list)) return(NULL)

  unassigned_vars <- setdiff(names(var_list), level_names)

  if (length(unassigned_vars) > 0) {
    if (is_multilevel) {
      stop(paste0(
        "You defined a design with multiple levels/dimensions, but provided unassigned ", arg_name, "-variables: ",  paste(unassigned_vars, collapse = "', '"),
        ". Please explicitly assign them to a target unit (e.g., ", level_names[1], " = list(", unassigned_vars[1], " = ...))."
      ), call. = FALSE)
    } else {
      auto_wrapped <- list()
      auto_wrapped[[level_names[1]]] <- var_list
      return(auto_wrapped)
    }
  }
  return(var_list)
}

#' Define the Structure of a Planned Experimental Design
#'
#' @description
#' This is the primary setup function for any power simulation in `PowRPriori`.
#' It creates a special `PowRPriori_design` object that contains all the necessary
#' information about the variables, the hierarchical structure, and the sample size
#' of a planned study.
#'
#' @details
#' **Variable Specification:**
#' Variables can be specified in different formats depending on their scale:
#' Nominal variables (e.g. a group variable with levels "control" and "treatment")
#' can be specified as factors (`group = factor(c("control", "treatment"))`) or
#' as character vectors (`group = c("control", "treatment")`), which are automatically
#' converted to factors. Continuous variables can be specified via their expected mean
#' and standard deviation (`test_score = list(mean = 10, sd = 5)`). Additionally,
#' variables can be defined as fixed numerical vectors (`predictor = 1:4`).
#'
#' **Assignment of Variables:**
#' By default, if `between` variables are specified directly as a simple list
#' (e.g., `between = list(treatment = c("A", "B"))`), they are randomized at the
#' lowest level of the design (individual assignment).
#' If a `between` variable should be assigned at a higher cluster level
#' (e.g., cluster-randomization at the class level), it must be wrapped in a named list
#' corresponding to that specific analysis unit. You do not need to mimic the full hierarchical
#' structure of your design here (e.g., no need to write school = list(class = list(...))).
#' Simply wrap the predictor in a single list named after the exact cluster level it belongs to
#' (see the nested design example below).
#'
#' `within` variables, on the other hand, are always crossed with the level-1
#' analysis units, effectively creating repeated measures for the lowest level.
#'
#' For a full tutorial and more complex design structures, see the package vignette:
#' `vignette("Workflow-Example", package = "PowRPriori")`.
#'
#' @param sample_size A named list specifying the building blocks and dimensions of the
#'   planned study sample (e.g. `list(class = 10, pupil = 20)`). This dictates which
#'   analysis units exist and the number of elements within each unit.
#' @param between A list of between-subject variables. These are variables where a given
#'   unit is assigned to exactly one level of the variable (e.g., participants or
#'   entire clusters being assigned to either an intervention or a control group).
#' @param within A list of within-subject variables. These are variables where all levels
#'   are observed within the same unit (e.g., repeated pre- and post-measurements within participants).
#'
#' @return A `PowRPriori_design` object containing the parsed design specifications.
#' @export
#'
#' @examples
#' # Simple 2x2 mixed design
#' simple_design <- define_design(
#'   sample_size = list(subject = 20),
#'   between = list(group = c("Control", "Treatment")),
#'   within = list(time = c("pre", "post"))
#' )
#'
#' # A nested (cluster-randomized) design where the intervention
#' # is assigned at the class level.
#' nested_design <- define_design(
#'   sample_size = list(class = 10,
#'                       pupil = 20),
#'   between = list(
#'     class = list(intervention = c("yes", "no")),
#'     pupil = list(support = c("yes", "no"))
#'   )
#' )
define_design <- function(sample_size, between = NULL, within = NULL) {
  if (!is.list(sample_size) || is.null(names(sample_size))) {
    stop("'sample_size' must be a named list (e.g., list(school=10, pupil=15)) with at least one entry.", call. = FALSE)
  }

  level_names <- names(sample_size)
  is_multilevel <- length(level_names) > 1

  design <- list(
    sample_size = sample_size,
    between = .check_and_wrap(between, "between", level_names, is_multilevel),
    within = .check_and_wrap(within, "within", level_names, is_multilevel)
  )

  class(design) <- "PowRPriori_design"
  return(design)
}

#' Get the Expected Fixed-Effects Structure
#'
#' @description
#' Analyzes a model formula and a design object to generate a template for the
#' `fixed_effects` parameter. This is a helper function designed to prevent
#' typos and ensure all necessary coefficients are specified. By default, this function prints a copy-paste-able code snippet
#' to the console, where the user only needs to fill in placeholders (`...`) for the values.
#'
#' @param formula An lme4-style model formula (e.g. `outcome ~ predictor1 * predictor2 + (1 | id)`).
#'                Since this function only uses the fixed-effects part of the model, specifying the random effects
#'                is optional here.
#' @param design A `PowRPriori_design` object created with `define_design()`.
#'
#' @return Invisibly returns a named list with placeholders, which can be used
#'   as a template for the `fixed_effects` argument in `power_sim()`.
#' @export
#'
#' @examples
#' design <- define_design(
#'   sample_size = list(subject = 20),
#'   between = list(group = c("Control", "Treatment")),
#'   within = list(time = c("pre", "post"))
#' )
#' get_fixed_effects_structure(y ~ group * time, design)
get_fixed_effects_structure <- function(formula, design) {
  # Temporarily enforce standard dummy coding to prevent issues with global user settings
  old_opts <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old_opts), add = TRUE)

  formula <- as.formula(formula)

  all_available_vars <- list()
  for (name in names(design$sample_size)) {
    all_available_vars[[name]] <- NA
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
        all_available_vars <- c(all_available_vars, element)
        if(!level_name %in% names(all_available_vars)) {
          all_available_vars[[level_name]] <- factor("1")
        }
      } else {
        all_available_vars[[level_name]] <- element
      }
    }
  }

  formula_vars <- all.vars(formula)
  predictor_vars <- formula_vars[-1]
  defined_vars <- names(all_available_vars)
  missing_vars <- setdiff(predictor_vars, defined_vars)

  if (length(missing_vars) > 0) {
    stop("The following variables present in the formula are missing in the given 'design'-object:\n",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  processed_structure <- lapply(all_available_vars, .to_factor_safely)
  grid_vars <- purrr::map(processed_structure, function(x) {
    if(length(x) == 1 && is.na(x)) return(1)
    if (is.factor(x) || is.numeric(x)) return(x[1])
    if (is.list(x)) return(x$mean)
    return(x)
  })
  dummy_data <- as.data.frame(grid_vars)
  fe_formula <- lme4::nobars(formula)
  outcome_name <- all.vars(fe_formula)[1]
  if (!outcome_name %in% names(dummy_data)) {
    dummy_data[[outcome_name]] <- 0
  }

  mm_colnames <- colnames(model.matrix(fe_formula, data = dummy_data))
  template_list <- as.list(rep("...", length(mm_colnames)))
  names(template_list) <- mm_colnames

  class(template_list) <- c("PowRPriori_fe_structure", "list")
  return(template_list)
}

#' @export
#' @noRd
print.PowRPriori_fe_structure <- function(x, ...){

  output_lines <- c("list(")
  item_names <- names(x)

  for (i in seq_along(item_names)) {
    name <- item_names[i]
    value <- x[[name]]

    if (grepl("[[:punct:]]| ", name)) {
      formatted_name <- paste0("`", name, "`")
    } else {
      formatted_name <- name
    }

    comma <- if (i < length(item_names)) "," else ""

    output_lines <- c(output_lines, paste0("  ", formatted_name, " = ", value, comma))
  }

  output_lines <- c(output_lines, ")")

  cat("Copy the following code and fill the placeholders (...) with your desired coefficients:\n\n")
  cat(paste("fixed_effects <-", paste(output_lines, collapse = "\n")))

  invisible(x)
}

#' Get the Expected Random-Effects Structure
#'
#' @description
#' Analyzes the random effects terms in a model formula and generates a template
#' for the specified `random_effects` parameters. This helps in specifying the required
#' standard deviations and correlations correctly. By default, this function prints a copy-paste-able code snippet
#' to the console, where the user only needs to fill in placeholders (`...`) for the values.
#'
#' @param formula An lme4-style model formula (e.g. `outcome ~ predictor1 * predictor2 + (1 | id)`).
#' @param design A `PowRPriori_design` object created with `define_design()`.
#' @param family The model family (`"gaussian"`, `"binomial"`, `"poisson"`).
#'   Determines how the residual variance in a design needs to be handled. Defaults to `"gaussian"`.
#'
#' @return Invisibly returns a nested list with placeholders, serving as a
#'   template for the `random_effects` argument in `power_sim()`.
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' design <- define_design(
#'   sample_size = list(subject = 20),
#'   within = list(time = c("pre", "post"))
#' )
#' get_random_effects_structure(y ~ time + (time|subject), design, family = "gaussian")
get_random_effects_structure <- function(formula, design, family = "gaussian") {
  # Temporarily enforce standard dummy coding to prevent issues with global user settings
  old_opts <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old_opts), add = TRUE)

  formula <- as.formula(formula)

  all_available_vars <- list()

  for (name in names(design$sample_size)) {
    all_available_vars[[name]] <- NA
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
        all_available_vars <- c(all_available_vars, element)
        if(!level_name %in% names(all_available_vars)) {
          all_available_vars[[level_name]] <- factor("1")
        }
      } else {
        all_available_vars[[level_name]] <- element
      }
    }
  }

  formula_vars <- all.vars(formula)
  predictor_vars <- formula_vars[-1]
  defined_vars <- names(all_available_vars)
  missing_vars <- setdiff(predictor_vars, defined_vars)

  if (length(missing_vars) > 0) {
    stop("The following variables present in the formula are missing in the design object: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  if (!any(family == c("gaussian", "binomial", "poisson"))) {
    stop("Unsupported family. Possible values for family are 'gaussian', 'binomial' and 'poisson'.", call. = FALSE)
  }

  all_available_vars <- lapply(all_available_vars, .to_factor_safely)

  temp_design <- design

  temp_design$sample_size <- lapply(temp_design$sample_size, function(x) 2)
  #if(is.null(temp_design$within)) temp_design$within <- list(dummy_within = c(1,2))

  dummy_data <- .create_design_matrix(temp_design, formula = formula)
  dummy_data[[all.vars(formula)[1]]] <- 1
  dummy_data2 <- dummy_data
  dummy_data$.dummy_obs <- 1
  dummy_data2$.dummy_obs <- 2
  dummy_data <- rbind(dummy_data, dummy_data2)
  # --------------------------------------

  re_terms <- lme4::findbars(formula)

  if (length(re_terms) > 0) {
    re_strings <- sapply(re_terms, function(x) paste0("(", paste(deparse(x), collapse = " "), ")"))
    safe_formula <- as.formula(paste(all.vars(formula)[1], "~ 1 +", paste(re_strings, collapse = " + ")))
  } else {
    safe_formula <- as.formula(paste(all.vars(formula)[1], "~ 1"))
  }

  template <- list()
  parsed_formula <- tryCatch(
    suppressMessages(suppressWarnings(
      lme4::glFormula(safe_formula, data = dummy_data)
    )),
    error = function(e) {
      message(e)
      NULL
    }
  )

  # Check for random effects
  if (!is.null(parsed_formula) && length(parsed_formula$reTrms$cnms) > 0) {

    for (i in seq_along(parsed_formula$reTrms$cnms)) {
      group <- names(parsed_formula$reTrms$cnms)[i]
      re_names <- parsed_formula$reTrms$cnms[[i]]

      term_list <- list()
      for (name in re_names) {
        term_list[[name]] <- "..."
      }

      if (length(re_names) > 1) {
        effect_pairs <- utils::combn(re_names, 2, simplify = FALSE)
        for (pair in effect_pairs) {
          cor_name <- paste0("cor_", gsub("[():`]", "", pair[1]), "__", gsub("[():`]", "", pair[2]))
          term_list[[cor_name]] <- "..."
        }
      }

      if (group %in% names(template)) {
        template[[group]] <- c(template[[group]], term_list)
      } else {
        template[[group]] <- term_list
      }
    }
  }

  if(family == "gaussian") template$sd_resid <- "..."

  class(template) <- c("PowRPriori_re_structure", "list")
  return(template)
}

#' @export
#' @noRd
print.PowRPriori_re_structure <- function(x, ...){
  output_lines <- c("list(")
  template_names <- names(x)

  for (i in seq_along(template_names)) {
    param_value <- x[[template_names[i]]]
    param_name <- ifelse(grepl("[():]", template_names[i]), paste0("`", template_names[i], "`"), template_names[i])

    if (!is.list(param_value)) {
      line <- paste0("  ", param_name, " = ", param_value)
      output_lines <- c(output_lines, line)
      next
    }

    output_lines <- c(output_lines, paste0("  ", param_name, " = list("))

    sub_list_names <- names(param_value)
    for (j in seq_along(sub_list_names)) {
      sub_name <- sub_list_names[j]
      sub_val <- param_value[[sub_name]]
      comma <- if (j < length(sub_list_names)) "," else ""
      formatted_name <- if (grepl("[():]", sub_name)) paste0("`", sub_name, "`") else sub_name

      output_lines <- c(output_lines, paste0("    ", formatted_name, " = ", sub_val, comma))
    }

    closing_bracket <- if (i < length(template_names)) "  )," else "  )"
    output_lines <- c(output_lines, closing_bracket)
  }

  output_lines <- c(output_lines, ")")

  cat("Copy the following structure and fill the placeholders (...) with your random effects values:\n\n")
  cat("random_effects <-", paste(output_lines, collapse = "\n"))

  invisible(x)
}


#' Calculate Fixed-Effects Coefficients from Mean Outcomes
#'
#' @description
#' A user-friendly helper function to translate expected outcomes (e.g., cell
#' means, probabilities, or rates) into the regression coefficients required
#' by the simulation. This is often more intuitive than specifying coefficients directly.
#'
#' @details
#' By default, this function applies effect coding (orthogonal contrasts) to the
#' predictors in your design (`center = TRUE`). Because the predictors are
#' treated as being mean-centered this way, the intercept of the calculated
#' coefficients represents the grand mean of your specified outcomes.
#'
#' Importantly, setting `center = TRUE` also adds an attribute to the resulting
#' coefficient list. When the coefficients are later passed to `power_sim()`,
#' the simulation engine will detect this attribute and automatically apply the corresponding
#' mean-centering to the generated sample data (grand-mean centering for between-subject
#' variables, and within-cluster / person-mean centering for within-subject variables).
#'
#' The choices for these respective centering techniques have been made to ensure the most robust
#' fixed / random effects and power estimates. For more details on the issue of centering, refer to the vignette:
#' `vignette("Workflow-Example", package = "PowRPriori")`
#'
#'
#' @param formula The fixed-effects part of the model formula (e.g., `y ~ group * time`).
#' @param outcome A data frame containing columns for all predictor variables and
#'   exactly one column for the expected outcome values.
#' @param center If `TRUE` (default), predictors in the design grid are
#'   effect-coded (i.e. the contrast coding is changed so that they are treated as being centered) to yield a grand-mean intercept.
#'   Additionally, the output is flagged to instruct `power_sim()` to automatically center the simulated data.
#'   If set to `FALSE`, the predictors are treated as not centered and no alterations to the contrast coding are applied.
#' @param family The model family (`"gaussian"`, `"binomial"`, `"poisson"`).
#'   The outcome values should be means for gaussian, probabilities (0-1) for
#'   binomial, and non-negative rates/counts for poisson.
#'
#' @return A named list of coefficients suitable for the `fixed_effects`
#'   argument in `power_sim()`.
#' @export
#'
#' @examples
#' outcome_means <- tidyr::expand_grid(
#'   group = c("Control", "Treatment"),
#'   time = c("pre", "post")
#' )
#' outcome_means$mean <- c(10, 10, 12, 15) # Specify expected means
#'
#' #Per default, the predictors are effect-coded (centered) here
#' fixed_effects_from_average_outcome(
#'   formula = score ~ group * time,
#'   outcome = outcome_means
#' )
fixed_effects_from_average_outcome <- function(formula, outcome, center = TRUE, family = "gaussian") {
  # Temporarily enforce standard dummy coding to prevent issues with global user settings
  old_opts <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old_opts), add = TRUE)

  formula <- as.formula(formula)
  fe_formula <- lme4::nobars(formula)

  predictor_vars <- all.vars(fe_formula)[-1]

  missing_cols <- setdiff(predictor_vars, names(outcome))
  if (length(missing_cols) > 0) {
    stop("The following columns specified in the formula are missing in the `outcome` dataframe: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  mean_col_name <- setdiff(names(outcome), predictor_vars)
  if (length(mean_col_name) != 1) {
    stop(
      "The `outcome` dataframe needs to have exactly one column defined for the values of the outcome variable.",
      call. = FALSE
    )
  }

  # 2. Factorize
  outcome_df <- as.data.frame(outcome)
  for (col in predictor_vars) {
    if (is.character(outcome_df[[col]])) {
      outcome_df[[col]] <- .to_factor_safely(outcome_df[[col]])
    }
  }

  if (center) {
    outcome_df <- .center_predictors(outcome_df, fe_formula)
    message("Note: 'center = TRUE'. The calculated fixed effects are based on grand-mean centered predictors. The intercept now represents the overall grand mean.")
  }

  # 3. Fit Model
  outcome_name <- all.vars(fe_formula)[1]
  names(outcome_df)[names(outcome_df) == mean_col_name] <- outcome_name

  if(family == "gaussian") {
    fit <- stats::lm(fe_formula, data = outcome_df)
  } else if (family == "binomial") {
    if(any(outcome_df[outcome_name] < 0) | any(outcome_df[outcome_name] > 1)) {
      stop("For `family = \"binomial\"`, values in the outcome column must be expected probabilities between 0 and 1.\nValues outside that range were found.",
           call. = F)
    } else {
      outcome_df[outcome_name] <- log(outcome_df[outcome_name] / (1 - outcome_df[outcome_name]))
      fit <- stats::lm(fe_formula, data = outcome_df)
    }
  } else if (family == "poisson") {
    if(any(outcome_df[outcome_name] < 0)){
      stop("For `family = \"poisson\"`, values in the outcome column must be expected, non-negative average counts.\nValues outside that range were found.",
           call. = F)
    }
    outcome_df[outcome_name] <- log(outcome_df[outcome_name])
    fit <- stats::lm(fe_formula, data = outcome_df)
  } else {
    stop("Unsupported family. Possible values for family are 'gaussian', 'binomial' and 'poisson'.", call. = FALSE)
  }

  return_parameters <- as.list(zapsmall(stats::coef(fit)))

  attr(return_parameters, "is_centered") <- center

  if(any(is.na(return_parameters))) stop("Error in parameter calculation. Check input formula and outcome dataframe for problems.")
  else return(return_parameters)
}

#' Summarize a Power Simulation Result
#'
#' @description
#' Provides a detailed and context-aware summary of a `PowRPriori` object. The output
#' includes the power table, parameter recovery diagnostics for fixed and random
#' effects, and (if applicable) calculated Intra-Class Correlations (ICCs). The output is
#' tailored for different model types (LM, LMM, GLMM).
#'
#' @param object An object of class `PowRPriori` returned by `power_sim()`.
#' @param ... Additional arguments (not used).
#'
#' @return Prints a formatted summary to the console.
#' @export
summary.PowRPriori <- function(object, ...) {

  if(object$critical_model_issues) {
    cat(
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
      "!!! CAUTION: The simulation was automatically canceled     !!!\n",
      "!!!          due to modelling issues.                      !!!\n",
      "!!! The following results are incomplete and unreliable.   !!!\n",
      "!!! They are solely presented to support diagnostics.      !!!\n",
      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n"
    )
  }

  cat("--- Power Simulation Results ---\n\n")
  cat(paste("Family:", object$family))
  cat("Formula:", deparse(object$formula), "\n\n")

  cat("\nPower Table:\n")
  print(object$power_table)

  cat("\n--- Parameter Recovery (Fixed Effects) ---\n")

  estimated_means <- colMeans(object$all_coefficients, na.rm = TRUE)

  true_effects <- unlist(object$parameters$fixed_effects)

  recovery_df_fe <- data.frame(
    True_Value = true_effects,
    Avg_Estimated = estimated_means[names(true_effects)],
    Bias = estimated_means[names(true_effects)] - true_effects
  )
  print(round(recovery_df_fe, 3))

  has_random_effects <- length(lme4::findbars(object$formula)) > 0
  spec <- object$parameters$random_spec
  all_re <- object$all_re_estimates

  if (has_random_effects) {
    # --- LMM or GLMM (Random Effects) ---
    cat("\n--- Parameter Recovery (Random Effects) ---\n")

    estimated_re <- colMeans(all_re, na.rm = TRUE)

    cat("\nStandard Deviations & Correlations:\n")
    if (!is.null(spec$icc_specs)) {
      true_variances <- lapply(spec$icc_specs, function(icc) icc * spec$overall_variance)
      var_resid <- spec$overall_variance - sum(unlist(true_variances))
      true_sds <- c(lapply(true_variances, sqrt), sd_resid = sqrt(var_resid))
      names(true_sds) <- c(paste0(names(spec$icc_specs), ".SD.(Intercept)"), "sd_resid")

      recovery_df_sd <- data.frame(
        Parameter = names(true_sds),
        True_Value = unlist(true_sds),
        Avg_Estimated = estimated_re[names(true_sds)],
        Bias = estimated_re[names(true_sds)] - unlist(true_sds),
        row.names = NULL
      )
      print(round(recovery_df_sd, 3))
    } else {
      true_re_list <- list()
      re_groups <- spec[!names(spec) == "sd_resid"]
      for(group in names(re_groups)){
        params <- re_groups[[group]]
        for(p_name in names(params)){
          if(grepl("^cor", p_name)){
            new_name <- paste0(group, ".", p_name)
            true_re_list[[new_name]] <- params[[p_name]]
          } else {
            new_name <- paste0(group, ".SD.", p_name)
            true_re_list[[new_name]] <- params[[p_name]]
          }
        }
      }
      if(!is.null(spec$sd_resid)) true_re_list$sd_resid <- spec$sd_resid
      true_re <- unlist(true_re_list)

      estimated_re <- colMeans(all_re, na.rm = TRUE)
      groups <- unique(sub("\\..*", "", names(estimated_re)))

      for (group in groups) {
        cat(paste0("\nGroup: ", group, "\n"))
        param_names <- names(estimated_re)[grepl(paste0("^", group), names(estimated_re))]
        display_names <- sub(paste0(group, "\\.(SD\\.|cor_)?"), "", param_names)

        recovery_df_re <- data.frame(
          Parameter = display_names,
          True_Value = true_re[param_names],
          Avg_Estimated = estimated_re[param_names],
          Bias = estimated_re[param_names] - true_re[param_names],
          row.names = NULL
        )

        numeric_cols_re <- sapply(recovery_df_re, is.numeric)
        recovery_df_re[numeric_cols_re] <- round(recovery_df_re[numeric_cols_re], 3)
        print(recovery_df_re)
      }
    }
    cat("\n\nIntra-Class Correlations (ICC):\n\n")

    theoretical_resid_var <- 0
    if(object$family == "binomial") theoretical_resid_var <- (pi^2)/3
    if(object$family == "poisson") theoretical_resid_var <- 1 # Vereinfachung, oft wird log(1+1/exp(Intercept)) genommen

    estimated_variances <- ifelse(grepl("\\.SD\\.|^sd_", names(estimated_re)), estimated_re^2, estimated_re)
    names(estimated_variances) <- names(estimated_re)
    re_variances <- estimated_variances[grepl("\\.SD\\.Intercept", names(estimated_variances))]
    total_variance_est <- sum(estimated_variances[grepl("\\.SD\\.", names(estimated_variances))]) +
      ifelse(object$family == "gaussian", estimated_variances["sd_resid"], theoretical_resid_var)
    estimated_iccs <- re_variances / total_variance_est
    names(estimated_iccs) <- sub("\\.SD\\.Intercept", "", names(re_variances))

    if (!is.null(spec$icc_specs)) {
      true_iccs <- unlist(spec$icc_specs)
    } else {
      true_sds <- unlist(spec)[!grepl("cor_", names(unlist(spec)))]
      true_variances <- true_sds^2
      true_re_variances <- true_variances[grepl("Intercept", names(true_variances))]
      true_total_variance <- sum(true_variances) + ifelse(object$family != "gaussian", theoretical_resid_var, 0)
      true_iccs <- true_re_variances / true_total_variance
      names(true_iccs) <- sub("\\.Intercept$", "", names(true_re_variances))
    }

    recovery_df_icc <- data.frame(
      Level = names(estimated_iccs),
      Implied_True_ICC = true_iccs[names(estimated_iccs)],
      Avg_Estimated_ICC = estimated_iccs,
      Bias = estimated_iccs - true_iccs[names(estimated_iccs)],
      row.names = NULL
    )
    numeric_cols_re <- sapply(recovery_df_icc, is.numeric)
    recovery_df_icc[numeric_cols_re] <- round(recovery_df_icc[numeric_cols_re], 3)
    print(recovery_df_icc)

  } else if (object$family == "gaussian") {
    # ---LM (Linear Model w/o Random Effects) ---
    cat("\n--- Parameter Recovery (Residuals) ---\n")

    estimated_sd <- mean(all_re$sd_resid, na.rm = TRUE)
    true_sd <- spec$sd_resid

    recovery_df_lm <- data.frame(
      Parameter = "sd_resid",
      True_Value = true_sd,
      Avg_Estimated = estimated_sd,
      Bias = estimated_sd - true_sd,
      row.names = NULL
    )
    recovery_df_lm[-1] <- round(recovery_df_lm[-1], 3)
    print(recovery_df_lm)
  }

  cat("\n\n--- Note ---\n")
  cat("Small bias values indicate that the simulation correctly\nestimated the a-priori specified effect sizes.\n")
}
