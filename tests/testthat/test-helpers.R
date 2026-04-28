test_that("fixed_effects_from_average_outcome works without centering (Dummy Coding)", {
  my_design <- define_design(
    sample_size = list(subject = 30),
    between = list(group = c("A", "B")),
    within = list(time = c("t1", "t2"))
  )

  my_formula <- y ~ group * time
  means_df_1 <- tidyr::expand_grid(
    group = c("Control", "Treatment"),
    time = c("pre", "post")
  )
  means_df_1$mean <- c(10, 12, 10, 15)

  coefficients_from_helper <- fixed_effects_from_average_outcome(my_formula, means_df_1, center = FALSE)
  expect_false(attr(coefficients_from_helper, "is_centered"))
  expect_named(coefficients_from_helper, c("(Intercept)", "groupTreatment", "timepost", "groupTreatment:timepost"))
  coeffcicients <- c(10, 0, 2, 3)
  expect_equal(unname(unlist(coefficients_from_helper)), coeffcicients)
})

test_that("fixed_effects_from_average_outcome correctly applies centering by default", {
  my_formula <- y ~ group * time
  expected_means <- tidyr::expand_grid(
    group = c("Control", "Treatment"),
    time = c("pre", "post")
  )
  expected_means$mean_score <- c(50, 52, 50, 60)

  res_centered <- fixed_effects_from_average_outcome(
    formula = my_formula,
    outcome = expected_means
  )

  expect_true(attr(res_centered, "is_centered"))

  expect_equal(unname(res_centered[["(Intercept)"]]), 53)

  expect_true("timepost" %in% names(res_centered))
  expect_true("groupTreatment:timepost" %in% names(res_centered))
})

test_that("fixed_effects_from_average_outcome works for binomial models", {
  outcome_probs <- tidyr::expand_grid(group = c("A", "B"), time = c("t1", "t2"))
  outcome_probs$prob <- c(0.5, 0.731, 0.5, 0.9)

  fe <- fixed_effects_from_average_outcome(
    formula = y ~ time * group,
    outcome = outcome_probs,
    family = "binomial",
    center = FALSE # center = FALSE, weil die Assertions unten auf Dummy-Coding basieren
  )

  expect_false(attr(fe, "is_centered"))
  expect_equal(unname(fe[["(Intercept)"]]), 0, tolerance = 1e-3)
  expect_equal(unname(fe[["timet2"]]), 1, tolerance = 1e-3)
})

test_that("get_fixed_effects_structure returns correct names", {
  # Veraltetes id = "sub" durch die neue sample_size Syntax ersetzt!
  design <- define_design(
    sample_size = list(sub = 10),
    between = list(group=c("A", "B")),
    within = list(time=c("A", "B"))
  )

  fe_names <- names(get_fixed_effects_structure(y ~ group * time, design))

  expect_equal(fe_names, c("(Intercept)", "groupB", "timeB", "groupB:timeB"))
})
