
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
  # `unique(x)` behält die Reihenfolge der ersten Erscheinung bei
  if (is.character(x)) {
    return(factor(x, levels = unique(x)))
  }
  # Wenn es schon ein Faktor ist oder etwas anderes, gib es unverändert zurück
  return(x)
}



#' Define the Experimental Structure of an Experimental Design
#'
#' @description
#' This is the primary setup function for any power simulation. It creates a
#' special `PSRtest_design` object that contains all the necessary information
#' about the variables and structure of your study.
#'
#' @details
#' Variables can be specified as different types. Nominal scale variables (e.g. group with levels "control" and "treatment")
#' can be specified as factors (`group = factor(c("control", "treatment"))`) or as character vectors (`c("control", "treatment")`),
#' in which case they are automatically converted to factors later on. Continuous variables can  be specified via mean
#' and standard deviation (`test_score = list(mean = 10, sd = 5)`). Additionally, variables can also be defined as numerical vectors
#' (`predictor = 1:4`).
#'
#' The `between` argument is particularly flexible. For simple designs, you can
#' provide a "flat" list of factors. For complex designs like cluster-randomized
#' trials, you can provide a hierarchical list to specify the level of
#' assignment (see examples). For a full tutorial, see the package vignette:
#' `vignette("Workflow-Example", package = "PowRPriori")`
#'
#' @param id A string specifying the name of the lowest-level unit of observation
#'   (e.g., `"subject"`, `"pupil"`, `plot_of_land`).
#' @param between A list of between-subject factors. Can be a simple list (for
#'   individual assignment) or a nested list (e.g., `list(class = list(group = ...))`)
#'   for group-level assignment.
#' @param within A list of within-subject factors. Each `id` will be measured
#'   at every level of these factors.
#' @param nesting_vars A list of variables that are only used for grouping in the
#'   random effects structure (e.g., `(1|school/class)`).
#'
#' @return A `PSRtest_design` object, which is a list containing the design specifications.
#' @export
#'
#' @examples
#' # Simple 2x2 mixed design
#' simple_design <- define_design(
#'   id = "subject",
#'   between = list(group = c("Control", "Treatment")),
#'   within = list(time = c("pre", "post"))
#' )
#'
#' # A nested (cluster-randomized) design where the intervention
#' # is assigned at the class level.
#' nested_design <- define_design(
#'   id = "pupil",
#'   between = list(
#'     class = list(intervention = c("yes", "no"))
#'   ),
#'   nesting_vars = list(class = factor(1:10))
#' )
define_design <- function(id, between = NULL, within = NULL, nesting_vars = NULL) {

  # Erstelle die Liste, die alle Design-Infos enthält
  design <- list(
    id = id,
    between = between,
    within = within,
    nesting_vars = nesting_vars
  )

  # Weise dem Objekt eine eigene Klasse zu, damit S3-Methoden funktionieren
  class(design) <- "PSRtest_design"

  return(design)
}

#' Get the Expected Fixed-Effects Structure
#'
#' @description
#' Analyzes a model formula and a design object to generate a template for the
#' `fixed_effects` parameter. This is a helper function designed to prevent
#' typos and ensure all necessary coefficients are specified. By default, this function prints a copy-pasteable code snippet
#' to the console, where the user only needs to fill in placeholders (`...`) for the values.
#'
#' @param formula An lme4-style model formula (e.g. `outcome ~ predictor1 * predictor2 + (1 | id)`).
#'                Since this function only uses the fixed-effects part of the model, specifying the random effects
#'                is optional here.
#' @param design A `PSRtest_design` object created with `define_design()`.
#' @param as_code If `TRUE` (default), prints a copy-pasteable R code template
#'   to the console.
#'
#' @return Invisibly returns a named list with placeholders, which can be used
#'   as a template for the `fixed_effects` argument in `power_sim()`.
#' @export
#'
#' @examples
#' design <- define_design(
#'   id = "subject",
#'   between = list(group = c("Control", "Treatment")),
#'   within = list(time = c("pre", "post"))
#' )
#' get_fixed_effects_structure(y ~ group * time, design)
get_fixed_effects_structure <- function(formula, design, as_code = TRUE) {

  # --- 1. Setup aus dem design-Objekt extrahieren ---
  formula <- as.formula(formula)
  id_var <- design$id

  # Rekonstruiere die `data_structure`-Liste für die interne Logik
  data_structure <- c(
    setNames(list(NA), id_var),
    design$between,
    design$within
  )

  # --- 2. Eingaben validieren ---
  formula_vars <- all.vars(formula)
  predictor_vars <- formula_vars[-1]
  defined_vars <- names(data_structure)
  missing_vars <- setdiff(predictor_vars, defined_vars)

  if (length(missing_vars) > 0) {
    stop("The following variables present in the formula are missing in the given 'design'-object:\n",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  # --- 3. Dummy-Datensatz für die Analyse erstellen ---
  processed_structure <- lapply(data_structure, .to_factor_safely)
  grid_vars <- purrr::map(processed_structure, function(x) {
    if(length(x) == 1 && is.na(x)) return(1)
    if (is.factor(x) || is.numeric(x)) return(x[1])
    if (is.list(x)) return(x$mean)
    return(x)
  })
  dummy_data <- as.data.frame(grid_vars)

  # --- 4. Koeffizientennamen extrahieren und als Vorlage zurückgeben ---
  fe_formula <- lme4::nobars(formula)

  outcome_name <- all.vars(fe_formula)[1]
  if (!outcome_name %in% names(dummy_data)) {
    dummy_data[[outcome_name]] <- 0
  }

  mm_colnames <- colnames(model.matrix(fe_formula, data = dummy_data))

  # Erstelle eine Vorlagen-Liste mit Platzhaltern
  template_list <- as.list(rep("...", length(mm_colnames)))
  names(template_list) <- mm_colnames

  if(as_code) {
    output_lines <- c("list(")
    item_names <- names(template_list)

    for (i in seq_along(item_names)) {
      name <- item_names[i]
      value <- template_list[[name]]

      # Prüfe, ob der Name in Backticks (`) eingeschlossen werden muss
      if (grepl("[[:punct:]]| ", name)) { # Prüft auf Sonderzeichen oder Leerzeichen
        formatted_name <- paste0("`", name, "`")
      } else {
        formatted_name <- name
      }

      # Füge ein Komma hinzu, außer beim letzten Element
      comma <- if (i < length(item_names)) "," else ""

      # Erstelle die formatierte Zeile
      output_lines <- c(output_lines, paste0("  ", formatted_name, " = ", value, comma))
    }

    output_lines <- c(output_lines, ")")

    cat("Copy the following code and fill the placeholders (...) with your desired coefficients:\n\n")
    cat(paste("fixed_effects <-", paste(output_lines, collapse = "\n")))
  }
  return(invisible(template_list))
}

#' Get the Expected Random-Effects Structure
#'
#' @description
#' Analyzes the random effects terms in a model formula and generates a template
#' for the specified `random_effects` parameters. This helps in specifying the required
#' standard deviations and correlations correctly. By default, this function prints a copy-pasteable code snippet
#' to the console, where the user only needs to fill in placeholders (`...`) for the values.
#'
#' @param formula An lme4-style model formula (e.g. `outcome ~ predictor1 * predictor2 + (1 | id)`).
#' @param design A `PSRtest_design` object created with `define_design()`.
#' @param family The model family (`"gaussian"`, `"binomial"`, `"poisson"`).
#'   Determines if `sd_resid` should be included in the template.
#' @param as_code If `TRUE` (default), prints a copy-pasteable R code template
#'   to the console.
#'
#' @return Invisibly returns a nested list with placeholders, serving as a
#'   template for the `random_effects` argument in `power_sim()`.
#' @export
#'
#' @examples
#' design <- define_design(
#'   id = "subject",
#'   within = list(time = c("pre", "post"))
#' )
#' get_random_effects_structure(y ~ time + (time|subject), design)
get_random_effects_structure <- function(formula, design, family = "gaussian", as_code = TRUE) {

  formula <- as.formula(formula)

    data_structure <- c(
    setNames(list(NA), design$id),
    design$between,
    design$within
  )

  # --- 1. Eingaben validieren ---
  formula_vars <- all.vars(formula)
  predictor_vars <- formula_vars[-1]
  defined_vars <- names(data_structure)
  missing_vars <- setdiff(predictor_vars, defined_vars)

  if (length(missing_vars) > 0) {
    stop("Die folgenden Variablen aus der Formel fehlen in der 'data_structure'-Liste: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  if (!any(family == c("gaussian", "binomial", "poisson"))) {
    stop("Unsupported family. Possible values for family are 'gaussian', 'binomial' and 'poisson'.", call. = FALSE)
  }

  # --- 2. Dummy-Datensatz für die Analyse erstellen ---

  # Wandle character-Vektoren sicher in Faktoren um (erste Stufe wird Referenz)
  processed_structure <- lapply(data_structure, .to_factor_safely)

  if (!is.null(design$between)) {
    min_n <- prod(purrr::map_int(design$between, ~length(unique(.))))
  } else {
    # Wenn es keine Between-Faktoren gibt, reichen 2 Probanden für die meisten Fälle
    min_n <- 2
  }

  dummy_data <- .create_design_matrix(design, current_n = min_n)
  dummy_data[[all.vars(formula)[1]]] <- 1

  template <- list()
  parsed_formula <- tryCatch(
    lme4::glFormula(formula, data = dummy_data),
    error = function(e) {
      message(e)
      NULL
      }
  )

  # Check for random effects
  if (!is.null(parsed_formula) && length(parsed_formula$reTrms$cnms) > 0) {

    # Get names of grouping variables
    grouping_vars <- names(parsed_formula$reTrms$cnms)

    for (group in grouping_vars) {
      # Get names for each grouping variable
      re_names <- parsed_formula$reTrms$cnms[[group]]

      # Erstelle die flache Liste für diesen Term
      term_list <- list()
      for (name in re_names) {
        term_list[[name]] <- "..."
      }

      # Füge Platzhalter für Korrelationen hinzu
      if (length(re_names) > 1) {
        effect_pairs <- utils::combn(re_names, 2, simplify = FALSE)
        for (pair in effect_pairs) {
          cor_name <- paste0("cor_", gsub("[():`]", "", pair[1]), "__", gsub("[():`]", "", pair[2]))
          term_list[[cor_name]] <- "..."
        }
      }
      template[[group]] <- term_list
    }
  }

  if(family == "gaussian") template$sd_resid <- "..."

  # --- Teil 4: Output als handgeschriebenen Code formatieren ---
  if(as_code) {
    output_lines <- c("list(")
    template_names <- names(template)

    for (i in seq_along(template_names)) {
      param_value <- template[[template_names[i]]]
      param_name <- ifelse(grepl("[():]", template_names[i]), paste0("`", template_names[i], "`"), template_names[i])

      # Behandlung für sd_resid
      if (!is.list(param_value)) {
        line <- paste0("  ", param_name, " = ", param_value)
        output_lines <- c(output_lines, line)
        next
      }

      # Behandlung für Gruppierungsfaktoren
      output_lines <- c(output_lines, paste0("  ", param_name, " = list("))

      sub_list_names <- names(param_value)
      for (j in seq_along(sub_list_names)) {
        sub_name <- sub_list_names[j]
        sub_val <- param_value[[sub_name]]
        comma <- if (j < length(sub_list_names)) "," else ""
        # Füge Backticks hinzu, wenn der Name Sonderzeichen enthält
        formatted_name <- if (grepl("[():]", sub_name)) paste0("`", sub_name, "`") else sub_name

        output_lines <- c(output_lines, paste0("    ", formatted_name, " = ", sub_val, comma))
      }

      closing_bracket <- if (i < length(template_names)) "  )," else "  )"
      output_lines <- c(output_lines, closing_bracket)
    }

    output_lines <- c(output_lines, ")")

    cat("Copy the following structure and fill the placeholders (...) with your random effects values:\n\n")
    cat("random_effects <-", paste(output_lines, collapse = "\n"))
  }

  return(invisible(template))
}


#' Calculate Fixed-Effects Coefficients from Mean Outcomes
#'
#' @description
#' A user-friendly helper function to translate expected outcomes (e.g., cell
#' means, probabilities, or rates) into the regression coefficients required
#' by the simulation. This is often more intuitive than specifying coefficients directly.
#'
#' @param formula The fixed-effects part of the model formula (e.g., `y ~ group * time`).
#' @param outcome A data frame containing columns for all predictor variables and
#'   exactly one column for the expected outcome values.
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
#' fixed_effects_from_average_outcome(
#'   formula = score ~ group * time,
#'   outcome = outcome_means
#' )
fixed_effects_from_average_outcome <- function(formula, outcome, family = "gaussian") {

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
  outcome_df <- as.data.frame(outcome) # Sicherstellen, dass es ein data.frame ist
  for (col in predictor_vars) {
    if (is.character(outcome_df[[col]])) {
      outcome_df[[col]] <- .to_factor_safely(outcome_df[[col]])
    }
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

  if(any(is.na(return_parameters))) stop("Error in parameter calculation. Check input formula and outcome dataframe for problems.")
  else return(return_parameters)
}

#' Summarize a Power Simulation Result
#'
#' @description
#' Provides a detailed and context-aware summary of a `PSRtest` object. The output
#' includes the power table, parameter recovery diagnostics for fixed and random
#' effects, and (if applicable) calculated Intra-Class Correlations (ICCs). The output is
#' tailored for different model types (LM, LMM, GLMM).
#'
#' @param object An object of class `PSRtest` returned by `power_sim()`.
#' @param ... Additional arguments (not used).
#'
#' @return Prints a formatted summary to the console.
#' @export
summary.PSRtest <- function(object, ...) {

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
  cat("Power Table:\n")
  cat("Formula:", deparse(object$formula), "\n\n")
  print(object$power_table)

  cat("\n--- Parameter Recovery (Fixed Effects) ---\n")

  # Berechne die durchschnittlich geschätzten Koeffizienten
  estimated_means <- colMeans(object$all_coefficients, na.rm = TRUE)

  # Hole die "wahren", ursprünglich spezifizierten Koeffizienten
  true_effects <- unlist(object$parameters$fixed_effects)

  # Erstelle einen Vergleichs-Dataframe
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
    # --- Fall A: LMM oder GLMM (Modelle mit Random Effects) ---
    cat("\n--- Parameter Recovery (Random Effects) ---\n")

    estimated_re <- colMeans(all_re, na.rm = TRUE)

    # SDs & Korrelationen anzeigen (Logik von letzter Antwort)
    cat("\nStandard Deviations & Correlations:\n")
    if (!is.null(spec$icc_specs)) {
      # Fall 1: Aus ICCs herleiten
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
      # Fall 2: Direkt aus random_effects
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
      # Dann die Residuen-SD hinzufügen
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

    # Füge theoretische Residualvarianz für GLMMs hinzu
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

    # Berechnung der "wahren" ICCs
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
    # --- Fall B: LM (Lineares Modell ohne Random Effects) ---
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
