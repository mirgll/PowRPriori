#' Create the Design Matrix for a Simulation
#'
#' @description
#' An internal helper function that takes the design specification and a sample
#' size to generate a single data frame representing all observations for one
#' simulation run. It handles within-, between-, nested, and crossed factors.
#'
#' @param design A `PSRtest_design` object from `define_design()`.
#' @param current_n The sample size for which the design matrix should be generated.
#' @param n_is_total A boolean that controls how `current_n` is interpreted. `TRUE` assumes that the whole sample used for the simulation should be size
#' `current_n`, `FALSE` assumes that `current_n` specifies the size of each cell as defined by `design`.
#'
#' @return A tibble (data frame) with predictor variables.
#'
#' @importFrom tidyr expand_grid crossing
#' @importFrom purrr keep map map_int
#' @importFrom dplyr bind_cols left_join
.create_design_matrix <- function(design, current_n, n_is_total = TRUE) {

  # 1. Extract information from design object
  id_var       <- design$id
  within_vars  <- lapply(design$within, .to_factor_safely)
  nesting_vars <- lapply(design$nesting_vars, .to_factor_safely)

  within_factors   <- purrr::keep(within_vars, ~!is.list(.))
  nesting_factors  <- purrr::keep(nesting_vars, ~!is.list(.))

  is_hierarchical <- is.list(design$between[[1]]) &&
    !all(c("mean", "sd") %in% names(design$between[[1]]))

  if (!is_hierarchical && length(design$between) > 0) {
    design$between <- setNames(list(design$between), id_var)
  }

  all_between_factors <- purrr::map(design$between, function(level_list) {
    purrr::keep(level_list, Negate(is.list))
  })

  # 2. Create between subjects structure
  if (!n_is_total) {
    if (length(nesting_factors) > 0) {
      n_groups <- nrow(tidyr::expand_grid(!!!nesting_factors))
      n_total <- current_n * n_groups
    } else {
      n_groups <- prod(purrr::map_int(unlist(all_between_factors, recursive = FALSE), length))
      if (n_groups == 0) n_groups <- 1
      n_total <- current_n * n_groups
    }
  } else {
    n_total <- current_n
  }

  subjects_df <- tibble::tibble(!!id_var := 1:n_total)

  if (length(nesting_factors) > 0) {
    nesting_combos <- tidyr::expand_grid(!!!nesting_factors)
    n_nesting_groups <- nrow(nesting_combos)

    n_per_nesting <- ceiling(n_total / n_nesting_groups)
    assigned_nesting <- nesting_combos[rep(seq_len(n_nesting_groups), each = n_per_nesting, length.out = n_total), ]
    subjects_df <- dplyr::bind_cols(subjects_df, assigned_nesting)
  }

  assignment_levels <- names(design$between)

  for (level in assignment_levels) {
    level_vars <- design$between[[level]]

    level_factors <- purrr::keep(level_vars, ~!is.list(.))
    level_continuous <- purrr::keep(level_vars, is.list)

    if (length(level_factors) > 0) {
      level_factors <- lapply(level_factors, .to_factor_safely)
    }

    if (length(level_factors) > 0) {
      level_combos <- tidyr::expand_grid(!!!level_factors)
      n_level_groups <- nrow(level_combos)

      if (level == id_var) {
        target_units <- subjects_df[[id_var]]
      } else {
        target_units <- unique(subjects_df[[level]])
      }

      n_units <- length(target_units)
      n_per_group <- floor(n_units / n_level_groups)
      remainder <- n_units %% n_level_groups
      group_indices <- rep(1:n_level_groups, times = n_per_group)
      if (remainder > 0) group_indices <- c(group_indices, 1:remainder)

      lookup_df <- tibble::tibble(
        !!level := target_units,
        level_combos[sample(group_indices), ]
      )

      subjects_df <- dplyr::left_join(subjects_df, lookup_df, by = level)
    }

    for (cont_var_name in names(level_continuous)) {
      params <- level_continuous[[cont_var_name]]

      if (level == id_var) {
        target_units <- subjects_df[[id_var]]
      } else {
        target_units <- unique(subjects_df[[level]])
      }

      lookup_df <- tibble::tibble(
        !!level := target_units,
        !!cont_var_name := rnorm(length(target_units), mean = params$mean, sd = params$sd)
      )
      subjects_df <- dplyr::left_join(subjects_df, lookup_df, by = level)
    }
  }

  if (length(within_factors) > 0) {
    design_df <- tidyr::crossing(subjects_df, !!!within_factors)
  } else {
    design_df <- subjects_df
  }

  return(design_df)
}

#' Simulate the Outcome Variable
#'
#' @description
#' An internal helper function that takes a complete design matrix and simulates
#' the dependent variable based on the specified fixed and random effects.
#'
#' @param design_df The data frame from `.create_design_matrix`.
#' @param formula The model formula.
#' @param fixed_effects A list of the fixed effects coefficients.
#' @param sds_random A list of the random effects' standard deviations and correlations.
#' @param family A string indicating the model family.
#'
#' @return The input `design_df` with an added column for the outcome variable.
#'
#' @importFrom lme4 nobars findbars glFormula
#' @importFrom MASS mvrnorm
#' @importFrom stats model.matrix rnorm rbinom rpois
.simulate_outcome <- function(design_df, formula, fixed_effects, sds_random, family = "gaussian") {
  # --- 1. Fixed-Effects-Komponente (Xβ) berechnen ---
  formula <- as.formula(formula)

  # Feste-Effekte-Formel extrahieren (ohne Zufallseffekte)
  fe_formula <- lme4::nobars(formula)

  # Design-Matrix (X) für die fixen Effekte erstellen
  design_df[[all.vars(fe_formula)[1]]] <- 0
  mm <- model.matrix(fe_formula, data = design_df)

  if(!identical(names(fixed_effects), colnames(mm))){
    mismatched_names <- setdiff(names(fixed_effects), colnames(mm))
    error_message <- paste0(
      "Die folgenden Namen in 'fixed_effects' passen nicht zum Modell: ",
      paste(mismatched_names, collapse = ", "),
      "\nTipp: Benutze die Funktion `get_fixed_effects_structure()` mit deiner Formel und Datenstruktur, um die korrekten Namen zu sehen."
    )
    stop(error_message, call. = F)
  }

  # Sicherstellen, dass die Reihenfolge der Koeffizienten übereinstimmt
  # Fehlende Koeffizienten werden auf 0 gesetzt (z.B. für Referenzkategorien)
  fe_vector <- rep(0, ncol(mm))
  names(fe_vector) <- colnames(mm)
  fe_vector[names(fixed_effects)] <- unlist(fixed_effects)

  # Matrix-Multiplikation, um den fixen Anteil des Prädiktors zu erhalten
  fixed_pred <- mm %*% fe_vector

  # --- 2. Random-Effects-Komponenten simulieren ---
  random_pred <- 0
  if(length(lme4::findbars(formula)) > 0) {
    random_effect_terms <- lme4::findbars(formula)

    if (!is.null(random_effect_terms)) {
      for (term in random_effect_terms) {
        grouping_var <- deparse(term[[3]])
        individual_group_vars <- strsplit(grouping_var, ":")[[1]]

        effects_formula <- as.formula(paste("~", deparse(term[[2]])))
        re_names <- colnames(model.matrix(effects_formula, data = design_df))

        params <- sds_random[[grouping_var]]

        unique_levels <- unique(design_df[, individual_group_vars, drop = FALSE])
        n_levels <- nrow(unique_levels)
        n_effects <- length(re_names)

        # --- Neue Logik zur Verarbeitung der "flachen" Parameter-Liste ---

        # 1. Trenne SDs von Korrelationen
        is_cor <- grepl("^cor", names(params))
        sds_list <- params[!is_cor]
        cor_list <- params[is_cor]

        # Stelle sicher, dass die SDs in der richtigen Reihenfolge sind
        sds_vector <- unlist(sds_list)[re_names]

        # 2. Baue die Korrelationsmatrix (R) aus der flachen Liste
        R <- diag(n_effects)
        colnames(R) <- rownames(R) <- re_names

        if (length(cor_list) > 0) {
          for (cor_name in names(cor_list)) {
            if (cor_name == "cor" && n_effects == 2) {
              R[1, 2] <- R[2, 1] <- cor_list[[cor_name]]
            } else {
              vars <- strsplit(sub("cor_", "", cor_name), "__")[[1]]
              clean_re <- gsub("[():`]", "", re_names)
              full_var1 <- re_names[which(clean_re == vars[1])]
              full_var2 <- re_names[which(clean_re == vars[2])]
              R[full_var1, full_var2] <- R[full_var2, full_var1] <- cor_list[[cor_name]]
            }
          }
        }

        # 3. Baue die Kovarianz-Matrix (Sigma) mit der D-R-D Formel
        if (n_effects == 1) {
          # Sonderfall: Nur ein Effekt, Kovarianz-Matrix ist einfach die Varianz
          cov_matrix <- matrix(sds_vector^2)
        } else {
          # Allgemeiner Fall: D-R-D Formel für mehrere Effekte
          D <- diag(sds_vector)
          cov_matrix <- D %*% R %*% D
        }

        # 4. Ziehe Zufallseffekte mit mvrnorm (Logik bleibt gleich)
        mu <- rep(0, n_effects)
        ran_effs <- as.matrix(MASS::mvrnorm(n = n_levels, mu = rep(0, n_effects), Sigma = cov_matrix))

        # Erstelle die Nachschlagetabelle und führe den Join durch
        ran_effs_df <- as.data.frame(ran_effs)
        colnames(ran_effs_df) <- paste0("re_", re_names)

        lookup_table <- dplyr::bind_cols(unique_levels, ran_effs_df)

        temp_df <- dplyr::left_join(design_df, lookup_table, by = individual_group_vars)

        # Berechne den Beitrag der Zufallseffekte für jede Beobachtung
        re_model_matrix <- model.matrix(effects_formula, data = temp_df)
        re_values_per_obs <- temp_df[, paste0("re_", re_names), drop = FALSE]

        random_pred <- random_pred + rowSums(re_values_per_obs * re_model_matrix)
      }
    }
  }

  eta <- fixed_pred + random_pred
  outcome_name <- all.vars(formula)[1]

  if (family == "gaussian") {
    # --- Linear Models (LM / LMM) ---

    resid_error <- stats::rnorm(nrow(design_df), mean = 0, sd = sds_random$sd_resid)
    y <- eta + resid_error

    design_df[[outcome_name]] <- as.vector(y)

  } else if (family == "binomial") {
    # ---Logistic Models (GLM / GLMM) ---

    # Inverse Logit-Funktion (Link-Funktion)
    p <- 1 / (1 + exp(-eta))

    # Simuliere binäre Daten (0 oder 1) basierend auf der Wahrscheinlichkeit p
    y <- stats::rbinom(n = nrow(design_df), size = 1, prob = p)

    design_df[[outcome_name]] <- y

  } else if (family == "poisson") {
    # --- Fall C: Poisson-Modelle (GLM / GLMM) ---
    # Der lineare Prädiktor wird in eine erwartete Zählrate (Lambda) umgewandelt.

    # Inverse Log-Funktion (Link-Funktion)
    lambda <- exp(eta)

    # Simuliere Zähldaten basierend auf der Rate lambda
    y <- stats::rpois(n = nrow(design_df), lambda = lambda)

    design_df[[outcome_name]] <- y

  } else {
    stop("Unsupported family. Possible values for family are 'gaussian', 'binomial' and 'poisson'.", call. = FALSE)
  }

  return(design_df)
}
