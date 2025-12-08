test_that("define_design creates the correct object structure", {
  # Create design object
  my_design <- define_design(
    id = "subject",
    between = list(group = c("A", "B")),
    within = list(time = c("t1", "t2"))
  )


  expect_s3_class(my_design, "PowRPriori_design")
  expect_equal(my_design$id, "subject")
  expect_true(is.list(my_design$between))
  expect_equal(names(my_design$between), "group")
  expect_equal(names(my_design$within), "time")
})

test_that("define_design handles hierarchical (cluster) designs", {

  design <- define_design(
    id = "pupil",
    between = list(
      class = list(intervention = c("yes", "no"))
    ),
    nesting_vars = list(class = 1:10)
  )

  expect_s3_class(design, "PowRPriori_design")
  expect_equal(names(design$between), "class")
  expect_true(is.list(design$between$class))
  expect_equal(names(design$between$class), "intervention")
})

test_that("define_design handles continuous variables", {

  design <- define_design(
    id = "subject",
    between = list(age = list(mean = 25, sd = 5))
  )

  expect_true(is.list(design$between$age))
  expect_equal(names(design$between$age), c("mean", "sd"))
  expect_equal(design$between$age$mean, 25)
})

test_that("internal .create_design_matrix creates correct matrices from designs", {
  my_design1 <- define_design(
    id = "subject",
    between = list(group = c("A", "B"),
                   iq = list(mean = 100, sd = 15)),
    within = list(time = c("t1", "t2"))
  )
  design_matrix1 <- PowRPriori:::.create_design_matrix(my_design1, current_n = 30, n_is_total = F)
  expect_equal(nrow(design_matrix1), 120)
  design_matrix2 <- PowRPriori:::.create_design_matrix(my_design1, current_n = 30, n_is_total = T)
  expect_equal(nrow(design_matrix2), 60)

  my_design2 <- define_design(
    id = "subject",
    between = list(group = c("A", "B"),
                   iq = list(mean = 100, sd = 15)),
    within = list(time = c("t1", "t2")),
    nesting_vars = list(class = factor(1:5), school = factor(1:2))
  )
  design_matrix3 <- PowRPriori:::.create_design_matrix(my_design2, current_n = 10, n_is_total = F)
  expect_equal(nrow(design_matrix3), 200)
})
