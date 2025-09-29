#' Visualize Simulation Data or Power Simulation Results
#'
#' @description
#' Generic plotting function with methods for different objects.
#' - When used on an lme4-style `formula`, it simulates and plots a single plausible dataset.
#' - When used on a `PowRPriori` object, it plots either a power curve from the object or a dataset from the simulation.
#'
#' The plotting of the dataset is designed to aid in evaluating whether the simulated data is plausible in the context
#' of the desired study design and model specifications. It can help determine whether the chosen parameters are sensible or might
#' need some adapting. The power curve, plotted from the resulting `PowRPriori` object of the `power_sim` function visualizes the iterations
#' of the simulation across the different sample sizes for which the power was calculated during simulation.
#'
#' @details
#' The parameters `x_var, group_var, color_var and facet_var` are `NULL` by default. If left `NULL`, they are automatically extracted from the `PowRPriori` object
#' or the `design` object.
#'
#'
#' @param x The object to plot.
#' @param ... Additional arguments (not used).
#' @param type The type of plot to create. For `PowRPriori` objects, can be
#'   `"power_curve"` or `"data"`.
#' @param design A `PowRPriori_design` object.
#' @param fixed_effects,random_effects Lists of effect parameters.
#' @param family The model family (e.g., `"gaussian"`).
#' @param n The sample size to simulate.
#' @param x_var,group_var,color_var,facet_var Strings specifying variables for plot aesthetics.
#' @param n_data_points The maximum number of trajectories in spaghetti plots.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' # 1. Plot prior to simulation to check data plausibility
#' design <- define_design(
#'   id = "subject",
#'   between = list(group = c("Control", "Treatment")),
#'   within = list(time = c("pre", "post"))
#' )
#'
#' fixed_effects <- list(
#'   `(Intercept)` = 10,
#'   groupTreatment = 2,
#'   timepost = 1,
#'   `groupTreatment:timepost` = 1.5
#' )
#'
#' random_effects <- list(
#'   subject = list(`(Intercept)` = 3),
#'   sd_resid = 5
#' )
#'
#' plot_sim_model(
#'   y ~ group * time + (1|subject),
#'   design = design,
#'   fixed_effects = fixed_effects,
#'   random_effects = random_effects,
#'   n = 30
#' )
#' \dontrun{
#' # 2. Plot from PowRPriori object after simulation
#'   power_results <- power_sim(
#'     formula = y ~ group * time + (1|subject),
#'     design = design,
#'     fixed_effects = fixed_effects,
#'     random_effects = random_effects,
#'     test_parameter = "groupTreatment:timepost",
#'     n_start = 20,
#'     n_increment = 5,
#'     n_sims = 100 # Using a smaller n_sims for a quick example
#'   )
#'
#'   # Power curve
#'   plot_sim_model(power_results, type = "power_curve")
#'
#'   # Plot sample data with automated aesthetics extraction
#'   plot_sim_model(power_results, type = "data")
#' }
plot_sim_model <- function(x, type, design, fixed_effects, random_effects, family,
                           n, x_var, group_var, color_var, facet_var,
                           n_data_points, ...) {
  UseMethod("plot_sim_model")
}

#' @param n The total sample size to simulate for the plot.
#'
#' @rdname plot_sim_model
#' @export
plot_sim_model.formula <- function(x, type = NULL, design, fixed_effects, random_effects, family = "gaussian",
                                   n, x_var = NULL, group_var = NULL,
                                   color_var = NULL, facet_var = NULL,
                                   n_data_points = 10, ...) {
  sim_data <- .create_design_matrix(design = design, current_n = n) %>%
    .simulate_outcome(formula = x, fixed_effects = fixed_effects,
                      sds_random = random_effects)

  .plot_data(data = sim_data, design = design, formula = x, x_var = x_var, family = family,
                         group_var = group_var, color_var = color_var,
                         facet_var = facet_var, n_data_points = n_data_points)
}


#' @param type The type of plot to create: `"power_curve"` (default) or `"data"`
#'   (to visualize the sample data from the simulation).
#'
#' @rdname plot_sim_model
#' @export
plot_sim_model.PowRPriori <- function(x, type = "power_curve", design = NULL, fixed_effects = NULL,
                                      random_effects = NULL, family = NULL, n = NULL,
                                      x_var = NULL, group_var = NULL,
                                      color_var = NULL, facet_var = NULL,
                                      n_data_points = 10, ...) {
  if (type == "power_curve") {
    power_data <- x$power_table
    n_var <- names(power_data)[1]

    plot_data_long <- power_data %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with(c("power", "ci_lower", "ci_upper")),
                   names_pattern = "(power|ci_lower|ci_upper)_(.*)",
                   names_to = c(".value", "parameter"))

    dodge <- ggplot2::position_dodge(width = ifelse(length(unique(plot_data_long$parameter))>1, 0.8, 0))
    p <- ggplot2::ggplot(plot_data_long,
                         ggplot2::aes(x = .data[[n_var]],
                                      y = .data$power,
                                      color = .data$parameter,
                                      group = .data$parameter)) +
      ggplot2::geom_line(linewidth = 1, position=dodge) +
      ggplot2::geom_point(size = 1.5, position=dodge) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$ci_lower, ymax=.data$ci_upper), width=.4, position=dodge) +
      ggplot2::geom_hline(yintercept = x$power_crit, linetype = "dashed", color = "red") +
      ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0,1,0.1)) +
      ggplot2::scale_x_continuous(limits = c(min(plot_data_long[[n_var]]) - 1, max(plot_data_long[[n_var]]) + 1)) +
      ggplot2::labs(title = "Power-Curve", y = "Power", x = n_var, color = "Parameter") +
      ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
  } else if (type == "data") {
    p <- .plot_data(data = x$sample_data, design = x$design, formula = x$formula, family = x$family,
                    x_var = x_var, group_var = group_var, color_var = color_var,
                    facet_var = facet_var, n_data_points = n_data_points)
  } else {
    stop("Parameter 'type' can only be either 'power_curve' or 'data'", call. = F)
  }
  return(p)
}

#' Internal Data Plotting Engine
#'
#' @description
#' An internal helper function containing the logic to "intelligently" create
#' plots from simulated data. It automatically chooses between spaghetti plots and jitter/point-range plots depending on
#' the specified design and model family.It derives sensible defaults for plot aesthetics from the design,
#' if they are not supplied directly via the `plot_sim_model` function.
#'
#' @param data The data frame to plot.
#' @param design The `PowRPriori_design` object.
#' @param formula An lme4-style formula (e.g. `outcome ~ predictor1 * predictor2 + (1 | subject)`)
#' @param family The model family (e.g., `"gaussian"`).
#' @param x_var,group_var,color_var,facet_var Strings specifying variables for plot aesthetics.
#' @param n_data_points The maximum number of trajectories in spaghetti plots.
#'
#' @return A `ggplot` object.
.plot_data <- function(data, design, formula, family, x_var, group_var,
                                   color_var, facet_var, n_data_points) {
  y_var <- all.vars(formula)[1]
  id_var <- design$id
  between_vars <- names(design$between)
  within_vars <- names(design$within)

  if(!is.null(within_vars)) {
    if (is.null(group_var)) {
      group_var <- id_var
    }
    if (is.null(x_var)) {
      x_var <- within_vars[1]
    }
    if (is.null(facet_var) && is.null(color_var)) {
      facet_var <- between_vars[1]
    }

    vars_to_average <- setdiff(within_vars, c(group_var, x_var, color_var, facet_var))

    if (length(vars_to_average) > 0) {
      message(paste0("Note: Effects of '", paste(vars_to_average, collapse = ", "), "' averaged for plot."))
      data <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(unlist(purrr::compact(list(group_var, x_var, color_var, facet_var)))))) %>%
        dplyr::summarise(!!y_var := mean(.data[[y_var]]), .groups = "drop")
    }
    y_var <- all.vars(formula)[1]

    if(family == "binomial") {
      grouping_vars <- purrr::compact(list(x_var, color_var, facet_var, y_var))

      count_data <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(unlist(grouping_vars)))) %>%
        dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")

      p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
        ggplot2::geom_jitter(ggplot2::aes(color = if(!is.null(color_var)) .data[[color_var]] else NULL),
                             width = 0.25, height = 0.15, alpha = 0.25, size = 1.5) +
        ggplot2::geom_point(
          data = count_data,
          ggplot2::aes(size = .data$n_obs^2, fill = if(!is.null(color_var)) .data[[color_var]] else NULL),
          shape = 23,
          alpha = 1, fill = "deepskyblue3", color = "black", stroke = 0.8
        ) +
        ggplot2::labs(title = paste("Jitter-Plot for Simulated Data"), subtitle = paste("Family:", family),
                      x = x_var, y = y_var) +
        ggplot2::coord_cartesian(ylim = c(-0.1, 1.1)) +
        ggplot2::scale_y_continuous(breaks = c(0, 1)) +
        ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")

      if (!is.null(facet_var)) {
        p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]))
      }
    } else {
      if (!is.null(facet_var)) {
        plot_spaghetti_data <- data %>%
          dplyr::group_by(.data[[facet_var]]) %>%
          dplyr::filter(.data[[group_var]] %in% sample(unique(.data[[group_var]]),
                                                       min(n_data_points, dplyr::n_distinct(.data[[group_var]])))) %>%
          dplyr::ungroup()
      } else if (length(unique(data[[group_var]])) > n_data_points) {
        sampled_groups <- sample(unique(data[[group_var]]), n_data_points)
        plot_spaghetti_data <- data[data[[group_var]] %in% sampled_groups, ]
      } else {
        plot_spaghetti_data <- data
      }

      p <- ggplot2::ggplot(plot_spaghetti_data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
      if (!is.null(facet_var)) {
        p <- p + ggplot2::geom_line(ggplot2::aes(group = .data[[group_var]]), alpha = 0.4, color = "grey50") +
          ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "line", linewidth = 1.2, color = "black") +
          ggplot2::facet_wrap(rlang::syms(facet_var)) +
          ggplot2::theme(legend.position = "none")
      } else if (!is.null(color_var)) {
        p <- p + ggplot2::geom_line(ggplot2::aes(group = .data[[group_var]], color = .data[[color_var]]), alpha = 0.6) +
          ggplot2::stat_summary(ggplot2::aes(group = .data[[color_var]], color = .data[[color_var]]),
                                fun = mean, geom = "line", linewidth = 1.2)
      } else {
        p <- p + ggplot2::geom_line(ggplot2::aes(group = .data[[group_var]]), alpha = 0.4) +
          ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "line", linewidth = 1.2, color = "black")
      }
      p <- p + ggplot2::labs(title = paste("Spaghetti-Plot for Simulated Data"), x = x_var, y = y_var, color = color_var) +
        ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
    }
  } else {
    if (is.null(x_var)) {
      x_var <- names(design$between)[1]
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.2) +
      ggplot2::stat_summary(fun = mean, geom = "point", size = 4, shape = 18) +
      ggplot2::stat_summary(
        fun.min = function(y) mean(y) - 1.96 * sd(y) / sqrt(length(y)),
        fun.max = function(y) mean(y) + 1.96 * sd(y) / sqrt(length(y)),
        geom = "errorbar",
        width = 0.2
      ) +
      ggplot2::labs(title = paste("Simulated Distribution"), x = x_var, y = y_var) +
      ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
  }

  return(p)
}
