#' Create the Design Matrix for a Simulation
#'
#' @description
#' An internal helper function that takes the design specification and an lme4-style formula to generate a
#' design matrix representing the structure of the experimental design.
#'
#'
#' @param design A `PowRPriori_design` object from `define_design()`.
#' @param formula The lme4-style model formula.
#'
#' @return A tibble (data frame) with predictor variables.
#'
#' @importFrom tidyr expand_grid crossing
#' @importFrom purrr keep map map_int
#' @importFrom dplyr bind_cols left_join
.create_design_matrix <- function(design, formula = NULL) {

  base_grid <- lapply(design$sample_size, seq_len)
  design_df <- tidyr::expand_grid(!!!base_grid)

  design_df[[".PowR_id"]] <- 1:nrow(design_df)

  # 2. Extract nesting structure from formula
  nesting_map <- list()

  if (!is.null(formula)) {
    bars <- lme4::findbars(as.formula(formula))
    valid_levels <- names(design$sample_size)

    all_groupings <- list()
    if (length(bars) > 0) {
      for (b in bars) {
        grouping_str <- deparse(b[[3]])
        tokens <- trimws(unlist(strsplit(grouping_str, "[/:]")))

        tokens <- intersect(tokens, valid_levels)

        if (length(tokens) > 0) {
          all_groupings <- c(all_groupings, list(tokens))
        }
      }
    }

    for (lvl in valid_levels) {
      containing_groupings <- purrr::keep(all_groupings, ~ lvl %in% .x)

      if (length(containing_groupings) > 0) {
        lengths <- sapply(containing_groupings, length)
        nesting_map[[lvl]] <- containing_groupings[[which.min(lengths)]]
      } else {
        nesting_map[[lvl]] <- lvl
      }
    }
  }

  # 3. Assign between variables
  if (!is.null(design$between)) {

    is_hierarchical <- is.list(design$between[[1]]) &&
      !all(c("mean", "sd") %in% names(design$between[[1]]))

    if (!is_hierarchical && length(design$between) > 0) {
      design$between <- stats::setNames(list(design$between), ".PowR_id")
    }

    for (level in names(design$between)) {
      level_vars <- design$between[[level]]
      level_factors <- purrr::keep(level_vars, ~!is.list(.))
      level_continuous <- purrr::keep(level_vars, is.list)

      grouping_cols <- level
      if (!is.null(nesting_map[[level]])) {
        grouping_cols <- nesting_map[[level]]
      }

      target_units <- unique(design_df[, grouping_cols, drop = FALSE])
      n_groups <- nrow(target_units)

      if (length(level_factors) > 0) {
        level_factors <- lapply(level_factors, .to_factor_safely)
        level_combos <- tidyr::expand_grid(!!!level_factors)
        n_combos <- nrow(level_combos)

        n_per_combo <- floor(n_groups / n_combos)
        remainder <- n_groups %% n_combos
        combo_indices <- rep(1:n_combos, times = n_per_combo)
        if (remainder > 0) combo_indices <- c(combo_indices, 1:remainder)

        lookup_df <- dplyr::bind_cols(
          target_units,
          level_combos[sample(combo_indices), , drop = FALSE]
        )
        design_df <- dplyr::left_join(design_df, lookup_df, by = grouping_cols)
      }

      for (cont_var_name in names(level_continuous)) {
        params <- level_continuous[[cont_var_name]]
        lookup_df <- target_units
        lookup_df[[cont_var_name]] <- stats::rnorm(n_groups, mean = params$mean, sd = params$sd)
        design_df <- dplyr::left_join(design_df, lookup_df, by = grouping_cols)
      }
    }
  }

  # 4. Within variables
  if (!is.null(design$within)) {
    flat_within <- list()
    for (lvl in names(design$within)) {
      for (var_name in names(design$within[[lvl]])) {
        flat_within[[var_name]] <- design$within[[lvl]][[var_name]]
      }
    }

    within_vars <- lapply(flat_within, .to_factor_safely)
    within_factors <- purrr::keep(within_vars, ~!is.list(.))
    within_continuous <- purrr::keep(flat_within, is.list)

    if (length(within_factors) > 0) {
      design_df <- tidyr::crossing(design_df, !!!within_factors)
    }
    for (cont_var_name in names(within_continuous)) {
      params <- within_continuous[[cont_var_name]]
      design_df[[cont_var_name]] <- stats::rnorm(nrow(design_df), mean = params$mean, sd = params$sd)
    }
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
  formula <- as.formula(formula)

  fe_formula <- lme4::nobars(formula)

  design_df[[all.vars(fe_formula)[1]]] <- 0
  mm <- model.matrix(fe_formula, data = design_df)

  if(!identical(names(fixed_effects), colnames(mm))){
    mismatched_names <- setdiff(names(fixed_effects), colnames(mm))
    error_message <- paste0(
      "The following names in 'fixed_effects' don't fit with the specified model: ",
      paste(mismatched_names, collapse = ", "),
      "\nHint: Use `get_fixed_effects_structure()` with your specified formula and data structure to get the correct names."
    )
    stop(error_message, call. = F)
  }

  fe_vector <- rep(0, ncol(mm))
  names(fe_vector) <- colnames(mm)
  fe_vector[names(fixed_effects)] <- unlist(fixed_effects)

  fixed_pred <- mm %*% fe_vector

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

        is_cor <- grepl("^cor", names(params))
        sds_list <- params[!is_cor]
        cor_list <- params[is_cor]

        sds_vector <- unlist(sds_list)[re_names]

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

        if (n_effects == 1) {
          cov_matrix <- matrix(sds_vector^2)
        } else {
          D <- diag(sds_vector)
          cov_matrix <- D %*% R %*% D
        }

        mu <- rep(0, n_effects)
        ran_effs <- as.matrix(MASS::mvrnorm(n = n_levels, mu = rep(0, n_effects), Sigma = cov_matrix))
        ran_effs_df <- as.data.frame(ran_effs)
        colnames(ran_effs_df) <- paste0("re_", re_names)

        lookup_table <- dplyr::bind_cols(unique_levels, ran_effs_df)

        temp_df <- dplyr::left_join(design_df, lookup_table, by = individual_group_vars)

        re_model_matrix <- model.matrix(effects_formula, data = temp_df)
        re_values_per_obs <- temp_df[, paste0("re_", re_names), drop = FALSE]

        random_pred <- random_pred + rowSums(re_values_per_obs * re_model_matrix)
      }
    }
  }

  eta <- fixed_pred + random_pred
  outcome_name <- all.vars(formula)[1]

  if (family == "gaussian") {
    resid_error <- stats::rnorm(nrow(design_df), mean = 0, sd = sds_random$sd_resid)
    y <- eta + resid_error

    design_df[[outcome_name]] <- as.vector(y)

  } else if (family == "binomial") {
    # Inverse Logit
    p <- 1 / (1 + exp(-eta))
    y <- stats::rbinom(n = nrow(design_df), size = 1, prob = p)
    design_df[[outcome_name]] <- y

  } else if (family == "poisson") {

    # Inverse Log
    lambda <- exp(eta)
    y <- stats::rpois(n = nrow(design_df), lambda = lambda)
    design_df[[outcome_name]] <- y

  } else {
    stop("Unsupported family. Possible values for family are 'gaussian', 'binomial' and 'poisson'.", call. = FALSE)
  }

  return(design_df)
}
