#Test new define design
design.test.1 <- define_design(
  sample_size = list(subject = 20, school = 5),
  within = list(subject = list(measurement = c("pre", "post"))),
  between = list(subject = list(group = c("a", "b")))
)

formula.tmp <- outcome ~ measurement*group + (1|school/subject)

get_fixed_effects_structure(formula.tmp, design.test.1)
fixed_effects <- list(
  `(Intercept)` = 10,
  measurementpost = 2,
  groupb = 2,
  `measurementpost:groupb` = 4
)

get_random_effects_structure(formula.tmp, design.test.1)
random_effects <- list(
  `subject:school` = list(
    `(Intercept)` = 4
  ),
  school = list(
    `(Intercept)` = 4
  ),
  sd_resid = 1
)

p.res <- power_sim(formula = formula.tmp,
                   design = design.test.1,
                   fixed_effects = fixed_effects,
                   random_effects = random_effects,
                   along = "school",
                   n_start = NULL,
                   n_increment = 2,
                   n_sims = 200,
                   center = T,
                   parallel_plan = "sequential")

plot_sim_model(object = p.res, type = "power_curve")
summary(p.res)

#EMA Design
design.ema <- define_design(sample_size = list(subject = 10),
              within = list(ar1 = list(mean=0, sd=1),
                            assessments = paste0("EMA_", 1:56)),
              between = list(pred1 = list(mean=0, sd=1))
              )

my_formula <- score ~ 1 + ar1 + pred1 + (1|subject)

get_fixed_effects_structure(my_formula, design.ema)
fixed_effects.ema <- list(
  `(Intercept)` = 20,
  ar1 = 5,
  pred1 = 5
)

get_random_effects_structure(my_formula, design.ema)
random_effects.ema <- list(
  subject = list(
    `(Intercept)` = 2
  ),
  sd_resid = 2
)

power.ema.res <- power_sim(formula = my_formula,
                            design = design.ema,
                            fixed_effects = fixed_effects.ema,
                            random_effects = random_effects.ema,
                            along = NULL,
                            n_start = 10,
                            n_increment = 5,
                            n_sims = 200,
                            center = T,
                            parallel_plan = "sequential")
plot_sim_model(power.ema.res, type="power_curve")
plot_sim_model(power.ema.res, type="data", n_data_points = 2)
summary(power.ema.res)



















