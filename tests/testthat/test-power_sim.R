# Setup minimal objects for testing validation logic
design <- define_design(id = "subject", between = list(group = c("A", "B")))
formula_lmm <- y ~ group + (1|subject)
formula_lm <- y ~ group
fe <- list(Intercept = 1, groupB = 0.5)
re_lmm <- list(subject = list(Intercept = 2), sd_resid = 1)
re_lm <- list(sd_resid = 1)

test_that("power_sim stops if random effects are missing for LMM", {
  expect_error(
    power_sim(
      formula = formula_lmm,
      design = design,
      fixed_effects = fe
      # random_effects is missing
    )
  )
})

test_that("power_sim stops if sd_resid is missing for gaussian LM", {
  expect_error(
    power_sim(
      formula = formula_lm,
      design = design,
      fixed_effects = fe,
      random_effects = list() # sd_resid is missing
    )
  )
})

test_that("power_sim stops when using icc_specs with a random slope formula", {
  slope_formula <- y ~ group + (group|subject)

  expect_error(
    power_sim(
      formula = slope_formula,
      design = design,
      fixed_effects = fe,
      icc_specs = list(subject = 0.5), # Incorrect for slope model
      overall_variance = 10
    )
  )
})

test_that("power_sim stops for mismatched test_parameter", {
  expect_error(
    power_sim(
      formula = formula_lm,
      design = design,
      fixed_effects = fe,
      random_effects = re_lm,
      test_parameter = "this_does_not_exist"
    )
  )
})

test_that("power_sim stops for incomplete fixed_effects list", {
  incomplete_fe <- list(Intercept = 1)

  expect_error(
    power_sim(
      formula = formula_lm,
      design = design,
      fixed_effects = incomplete_fe,
      random_effects = re_lm
    )
  )
})
