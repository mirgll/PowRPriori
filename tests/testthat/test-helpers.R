test_that("coefficient from mean helper correctly calculates coefficients", {
  my_design <- define_design(
    id = "subject",
    between = list(group = c("A", "B")),
    within = list(time = c("t1", "t2"))
  )
  my_formula <- y ~ group * time
  means_df_1 <- tidyr::expand_grid(
    group = c("Control", "Treatment"),
    time = c("pre", "post")
  )
  means_df_1$mean <- c(10, 12, 10, 15)

  coeffcicients = c(10, 0, 2, 3)
  coefficients_from_helper = unlist(fixed_effects_from_average_outcome(my_formula, means_df_1))

  expect_named(coefficients_from_helper, c("(Intercept)", "groupTreatment", "timepost", "groupTreatment:timepost"))
  names(coefficients_from_helper) <- NULL
  expect_equal(unlist(coefficients_from_helper), coeffcicients)
})

test_that("fixed_effects_from_average_outcome works for binomial models", {

  outcome_probs <- tidyr::expand_grid(group = c("A", "B"), time = c("t1", "t2"))
  outcome_probs$prob <- c(0.5, 0.731, 0.5, 0.9)

  fe <- fixed_effects_from_average_outcome(
    formula = y ~ time * group,
    outcome = outcome_probs,
    family = "binomial"
  )

  expect_equal(fe$`(Intercept)`, 0, tolerance = 1e-3)
  expect_equal(fe$timet2, 1, tolerance = 1e-3)
})

test_that("get_fixed_effects_structure returns correct names", {

  design <- define_design(id = "sub", between = list(group=c("A", "B")), within = list(time=c("A", "B")))

  fe_names <- names(get_fixed_effects_structure(y ~ group * time, design, as_code = FALSE))

  expect_equal(fe_names, c("(Intercept)", "groupB", "timeB", "groupB:timeB"))
})
