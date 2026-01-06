#### Test define_design
## Pure Within Subject
design.within <- define_design("subject", within = list(time = c("Pre","Post"), task = c("easy", "medium", "hard")))

## Pure Between Subject
design.between <- define_design("school", between = list(type = c("haupt", "real", "gymn")))

## Between and Within Subject
design.mixed <- define_design("subject", between = list(group = c("Treatment", "Placebo", "Control")),
                              within = list(time = c("Pre","Post"), task = c("easy", "medium", "hard")))

## Numerical Factors
design.numerical <- define_design("subject", between = list(group = c("Treatment", "Placebo", "Control")),
                                  within = list(time = 1:3))

## Continuous predictor
design.continuous <- define_design("subject", between = list(group = c("Treatment", "Placebo", "Control"),
                                                             iq = list(mean = 100, sd = 15)),
                                              within = list(time = c("pre", "post")))
tmp <- PowRPriori:::.create_design_matrix(design.continuous, 30)
## N_is_total test
design.n_is_total <- define_design("pupil", between = list(group = c("intervention", "control"),
                                                       iq = list(mean = 100, sd = 15)),
                               within = list(rep_meas = c("pre", "post")))
tmp <- PowRPriori:::.create_design_matrix(design.n_is_total, 30, n_is_total = F)


## Nested Designs
design.nested <- define_design("pupil", between = list(group = c("intervention", "control"),
                                                       iq = list(mean = 100, sd = 15)),
                                        within = list(rep_meas = c("pre", "post")),
                                        nesting_vars = list(class = factor(1:10)))
design.nested <- define_design("pupil", between = list(
                                                    class = list(group = c("intervention", "control")),
                                                    pupil = list(iq = list(mean = 100, sd = 15))),
                               within = list(rep_meas = c("pre", "post")),
                               nesting_vars = list(class = factor(1:10)))
tmp <- PowRPriori:::.create_design_matrix(design.nested, 5, n_is_total = T)

## One Sample Design
design.onesample <- define_design("subject")
tmp <- PowRPriori:::.create_design_matrix(design.onesample, 30, n_is_total = F)


#### Test get_effects and get_random_effects_structure
## Overall design
test.design.get_fx <- define_design("subject", between = list(group = c("Control", "Placebo", "Treatment"), group2 = c("Control", "Treament")),
                                    within = list(time = c("Pre","Post"), task = c("easy", "medium", "hard"), task2 = 1:3))
## Single random intercept
test.formula = y ~ group * time + (1 | subject)

get_fixed_effects_structure(formula = test.formula, design = test.design.get_fx)
get_random_effects_structure(formula = test.formula, design = test.design.get_fx)

## Single random slope (factorial), random intercept
test.formula = y ~ group * time + (task | subject)

get_fixed_effects_structure(formula = test.formula, design = test.design.get_fx)
get_random_effects_structure(formula = test.formula, design = test.design.get_fx)

## Single random slope (continuous), random intercept
test.formula = y ~ group2 * time + (task2 | subject)

get_fixed_effects_structure(formula = test.formula, design = test.design.get_fx)
get_random_effects_structure(formula = test.formula, design = test.design.get_fx)

## Two random slopes, random intercept
test.formula = y ~ group2 * time + (time + task | subject)

get_fixed_effects_structure(formula = test.formula, design = test.design.get_fx)
get_random_effects_structure(formula = test.formula, design = test.design.get_fx)

## Nested random intercept
test.formula = y ~ group2 * time + (1 | subject/task)

get_fixed_effects_structure(formula = test.formula, design = test.design.get_fx)
get_random_effects_structure(formula = test.formula, design = test.design.get_fx)


## Crossed random intercepts
test.formula = y ~ group2 * time + (1 | subject) + (1 | task)

get_fixed_effects_structure(formula = test.formula, design = test.design.get_fx)
get_random_effects_structure(formula = test.formula, design = test.design.get_fx)

## No fixed Intercept
test.formula = y ~ -1 + group2 * time + (1 | subject) + (1 | task)

get_fixed_effects_structure(formula = test.formula, design = test.design.get_fx)
get_random_effects_structure(formula = test.formula, design = test.design.get_fx)

#### Test effects_from_means
cat("--- Starting tests for fixed_effects_from_average_outcome ---\n\n")

# --- Standard Cases ("Happy Path") ---

cat("--- Test Case 1: Standard 2x2 design (interaction) ---\n")
test_formula_1 <- y ~ group * time
means_df_1 <- tidyr::expand_grid(
  group = c("Control", "Treatment"),
  time = c("pre", "post")
)
means_df_1$mean <- c(10, 12, 10, 15) # Control (10->12), Treatment (10->15)
tmp<-fixed_effects_from_average_outcome(formula = test_formula_1, outcome = means_df_1)
print(fixed_effects_from_average_outcome(formula = test_formula_1, outcome = means_df_1))
cat("\n--------------------------------------------------\n")


cat("--- Test Case 2: Additive model ---\n")
test_formula_2 <- y ~ group + time
# We use the same `means` dataframe
print(fixed_effects_from_average_outcome(formula = test_formula_2, outcome = means_df_1))
cat("\n--------------------------------------------------\n")


# --- Complex Designs and Formulas ---

cat("--- Test Case 3: Complex 2x2x2 design ---\n")
test_formula_3 <- y ~ group * time * gender
means_df_3 <- tidyr::expand_grid(
  group = c("Control", "Treatment"),
  time = c("pre", "post"),
  gender = c("m", "w")
)
means_df_3$mean <- c(10, 12, 10, 15, 11, 13, 11, 16) # Example means
print(fixed_effects_from_average_outcome(formula = test_formula_3, outcome = means_df_3))
cat("\n--------------------------------------------------\n")


cat("--- Test Case 4: Model without intercept (-1) ---\n")
test_formula_4 <- y ~ -1 + group * time
print(fixed_effects_from_average_outcome(formula = test_formula_4, outcome = means_df_1))
cat("\n--------------------------------------------------\n")


# --- Test for Flexibility ---

cat("--- Test Case 5: Flexible name for the outcome column ---\n")
means_df_5 <- means_df_1
names(means_df_5)[names(means_df_5) == "mean"] <- "expected_score"
print(fixed_effects_from_average_outcome(formula = test_formula_1, outcome = means_df_5))
cat("\n--------------------------------------------------\n")


# --- Test for Error Handling (Expected Errors) ---

cat("--- Test Case 6: Incomplete `means` dataframe (expects error) ---\n")
means_df_6 <- means_df_1[-1, ] # Remove one row
try(fixed_effects_from_average_outcome(formula = test_formula_1, outcome = means_df_6))
cat("\n--------------------------------------------------\n")


cat("--- Test Case 7: Missing predictor column (expects error) ---\n")
means_df_7 <- means_df_1
means_df_7$time <- NULL # Remove the `time` column
try(fixed_effects_from_average_outcome(formula = test_formula_1, outcome = means_df_7))
cat("\n--------------------------------------------------\n")


cat("--- Test Case 8: Ambiguous `means` dataframe (expects error) ---\n")
test_formula_8 <- y ~ group
means_df_8 <- data.frame(
  group = c("Control", "Treatment"),
  mean = c(10, 12),
  stderror = c(1.5, 1.8) # Additional, ambiguous column
)
try(fixed_effects_from_average_outcome(formula = test_formula_8, outcome = means_df_8))
cat("\n--------------------------------------------------\n")


# Test Case 9: Binomial (logistic) model
test_formula_9 <- y ~ group * time
means_df_9 <- tidyr::expand_grid(
  group = c("Control", "Treatment"),
  time = c("pre", "post")
)
means_df_9$prob <- c(0.5, 0.6, 0.5, 0.8) # Expected probabilities
fixed_effects_from_average_outcome(formula = test_formula_9, outcome = means_df_9, family = "binomial")
cat("\n--------------------------------------------------\n")


# Test Case 10: Poisson model
test_formula_10 <- y ~ group * time
means_df_10 <- tidyr::expand_grid(
  group = c("Control", "Treatment"),
  time = c("pre", "post")
)
means_df_10$rate <- c(5, 6, 5, 8) # Expected counts/rates
fixed_effects_from_average_outcome(formula = test_formula_10, outcome = means_df_10, family = "poisson")


#### Workflow for power_sim() Function
## Test with random effects parameter
test.design.get_fx <- define_design("subject", between = list(group = c("Control", "Placebo", "Treatment"), group2 = c("Control", "Treatment")),
                                    within = list(time = c("Pre","Post"), task = c("easy", "medium", "hard"), task2 = 1:3))
test.formula <- y ~ group2 * time + (time | subject)

get_fixed_effects_structure(test.formula, test.design.get_fx)

fixed_effects <- list(
  `(Intercept)` = 10,
  group2Treatment = 0,
  timePost = 1,
  `group2Treatment:timePost` = 1.4
)

get_random_effects_structure(test.formula, test.design.get_fx)

random_effects <- list(
  subject = list(
    timePost = 2,
    `(Intercept)` = 5,
    cor_Intercept__timePost = 0.2
  ),
  sd_resid = 4
)

power_results <- power_sim(formula = test.formula,
                           design = test.design.get_fx,
                           test_parameter = c("group2Treatment:timePost"),
                           fixed_effects = fixed_effects,
                           random_effects = random_effects,
                           power_crit = 0.8,
                           n_start = 50,
                           n_increment = 5,
                           n_sims = 200,
                           parallel_plan = "sequential", max_simulation_steps = 4
                           )

PowRPriori::plot_sim_model(power_results)
PowRPriori::plot_sim_model(power_results, type = "data", facet_var = "group2")

summary(power_results)

## test with ICC parameter
test.design.get_fx <- define_design("subject", between = list(group = c("Control", "Placebo", "Treatment"), group2 = c("Control", "Treatment")),
                                    within = list(time = c("Pre","Post"), task = c("easy", "medium", "hard"), task2 = 1:3))
test.formula <- y ~ group2 * time + (time | subject)

fixed_effects <- list(
  `(Intercept)` = 10,
  group2Treatment = 4,
  timePost = 0,
  `group2Treatment:timePost` = 6
)

iccs <- list(`subject` = 0.2)
overall_var <- 30

power_results <- power_sim(formula = test.formula,
                           design = test.design.get_fx,
                           test_parameter = c("group2Treatment", "group2Treatment:timePost"),
                           fixed_effects = fixed_effects,
                           icc_specs = iccs,
                           overall_variance = overall_var,
                           power_crit = 0.8,
                           n_start = 15,
                           n_increment = 5,
                           n_sims = 200,
                           parallel_plan = "sequential"
)

plot_sim_model(power_results, type = "power_curve")
plot_sim_model(power_results, type = "spaghetti", facet_var = "group2")
summary(power_results)

## Test for nested design
nested_design <- define_design(
  id = "pupil",
  between = list(
    class = list(intervention = c("yes", "no"))
  ),
  nesting_vars = list(class = factor(1:10))
)

nested_formula <- outcome ~ intervention + (1 | class/pupil)

get_fixed_effects_structure(nested_formula, nested_design)

fixed_nested_effects <- list(
  `(Intercept)` = ...,
  interventionno = ...
)

dummy_data <- PowRPriori:::.create_design_matrix(nested_design, 30, n_is_total = F)
dummy_data[[all.vars(nested_formula)[1]]] <- 1
dummy_data_2 <- dummy_data

dummy_data_2 <- dummy_data_2 %>%
  dplyr::select(c(nested_design$id, names(nested_design$nesting_vars))) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(),
                              function(x){
                                if(is.numeric(x)){
                                  x <- length(x) + x
                                } else if (is.factor(x)){
                                  x <- paste0(as.character(x), "_2")
                                }
                              })) %>%
  dplyr::bind_cols(., dummy_data %>% dplyr::select(!c(nested_design$id, names(nested_design$nesting_vars))))

dummy_data <- dplyr::bind_rows(dummy_data, dummy_data_2)


get_random_effects_structure(nested_formula, nested_design, family = "binomial")

## Workflow for linear model (no random effects)
# --- 1. Design Definition ---
# A simple between-subject design with two groups
lm_design <- define_design(
  id = "subject",
  between = list(group = c("Control", "Treatment"))
)

# --- 2. Formula & Effect Specification ---
lm_formula <- score ~ group
# We expect the control group to have a mean score of 10 and the treatment group a mean of 10.5
lm_means <- data.frame(
  group = c("Control", "Treatment"),
  mean_score = c(10, 11)
)
# Automatically calculate the required regression coefficients
lm_fixed_effects <- fixed_effects_from_average_outcome(
  formula = lm_formula,
  outcome = lm_means
)
# For a simple lm, we only need to specify the residual standard deviation
lm_random_effects <- list(sd_resid = 2.0)
# --- 3. Power Simulation ---
lm_results <- power_sim(
  formula = lm_formula,
  design = lm_design,
  fixed_effects = lm_fixed_effects,
  random_effects = lm_random_effects,
  test_parameter = "groupTreatment",
  family = "gaussian", # Specify the model family
  n_start = 50,
  n_increment = 10,
  power_crit = 0.80,
  n_sims = 500, # A higher number of sims for more stable estimates
  parallel_plan = "sequential"
  )
# --- 4. Summarize and Plot Results ---
summary(lm_results)
plot_sim_model(lm_results, type = "power_curve")
plot_sim_model(lm_results, type = "data")

## Workflow for generalized linear model (no random effects)
# --- 1. Design Definition ---
# A 2x2 between-within design
glm_design <- define_design(
  id = "subject",
  between = list(condition = c("A", "B")),
  within = list(time = c("pre", "post"))
)

# --- 2. Formula & Effect Specification ---
glm_formula <- solved ~ condition * time

# We expect probabilities of success
glm_probs <- tidyr::expand_grid(
  condition = c("A", "B"),
  time = c("pre", "post")
)
# Specify expected success probabilities for each of the 4 cells
glm_probs$probability <- c(0.5, 0.45, 0.5, 0.9)

# Automatically calculate coefficients on the log-odds scale
glm_fixed_effects <- fixed_effects_from_average_outcome(
  formula = glm_formula,
  outcome = glm_probs,
  family = "binomial"
)

# --- 3. Power Simulation ---
glm_results <- power_sim(
  formula = glm_formula,
  design = glm_design,
  fixed_effects = glm_fixed_effects,
  test_parameter = "conditionB:timepost",
  family = "binomial",
  n_start = 40,
  n_increment = 10,
  power_crit = 0.80,
  n_sims = 500,
  parallel_plan = "sequential"
)

# --- 4. Summarize and Plot Results ---
summary(glm_results)
plot_sim_model(glm_results, type = "power_curve")
plot_sim_model(glm_results, type = "data")

## Workflow GLMM (Poisson)
# --- 1. Design Definition ---
# A longitudinal design with 4 measurement points
glmer_design <- define_design(
  id = "subject",
  within = list(week = c(-1.5, -0.5, 0.5, 1.5), rep_measure = c("t1", "t2"))
)
#c(-1.5, -0.5, 0.5, 1.5)
# --- 2. Formula & Effect Specification ---
glmer_formula <- symptoms ~ week*rep_measure + (week | subject)

# We expect a certain number of symptoms (counts) per week
glmer_counts <- tidyr::expand_grid(week = 1:4, rep_measure = c("t1", "t2"))
glmer_counts$symptoms <- c(5.0, 4.5, 4.8, 4.3, 4.5, 4.1, 4.2, 3.4)


# Automatically calculate coefficients on the log scale
glmer_fixed_effects <- fixed_effects_from_average_outcome(
  formula = glmer_formula,
  outcome = glmer_counts,
  family = "poisson"
)

get_random_effects_structure(glmer_formula, glmer_design, family = "poisson")

# Specify the random effects (only a random intercept in this case)
glmer_random_effects <- list(
  subject = list(
    `(Intercept)` = 0.5,
    week = 2,
    cor_Intercept__week = 0.2
  )
)

# --- 3. Power Simulation ---
glmer_results <- power_sim(
  formula = glmer_formula,
  design = glmer_design,
  fixed_effects = glmer_fixed_effects,
  random_effects = glmer_random_effects,
  test_parameter = "week",
  family = "poisson",
  n_start = 30,
  n_increment = 5,
  power_crit = 0.80,
  n_sims = 200,
  parallel_plan = "sequential" # Explicitly set to sequential
)

# --- 4. Summarize and Plot Results ---
summary(glmer_results)
plot_sim_model(glmer_results, type = "power_curve")
plot_sim_model(glmer_results, type = "data") # "data" calls our intelligent spaghetti/jitter plot

## Workflow GLMM (Binomial)
# --- 1. Design Definition ---
# A longitudinal design with 4 measurement points
glmer_design <- define_design(
  id = "subject",
  between = list(condition = c("A", "B")),
  within = list(time = c("pre", "post"))
)

# --- 2. Formula & Effect Specification ---
glmer_formula <- symptoms ~ condition*time + (1 | subject)

# We expect probabilities of success
glm_probs <- tidyr::expand_grid(
  condition = c("A", "B"),
  time = c("pre", "post")
)
# Specify expected success probabilities for each of the 4 cells
glm_probs$probability <- c(0.5, 0.45, 0.5, 0.9)

# Automatically calculate coefficients on the log scale
glmer_fixed_effects <- fixed_effects_from_average_outcome(
  formula = glmer_formula,
  outcome = glm_probs,
  family = "binomial"
)

# Specify the random effects (only a random intercept in this case)
glmer_random_effects <- list(
  subject = list(`(Intercept)` = 0.5)
)

# --- 3. Power Simulation ---
glmer_results <- power_sim(
  formula = glmer_formula,
  design = glmer_design,
  fixed_effects = glmer_fixed_effects,
  random_effects = glmer_random_effects,
  test_parameter = "conditionB:timepost",
  family = "binomial",
  n_start = 500,
  n_increment = 5,
  power_crit = 0.80,
  n_sims = 2000,
  parallel_plan = "sequential"
)

# --- 4. Summarize and Plot Results ---
summary(glmer_results)
plot_sim_model(glmer_results, type = "power_curve")
plot_sim_model(glmer_results, type = "data") # "data" calls our intelligent spaghetti/jitter plot
