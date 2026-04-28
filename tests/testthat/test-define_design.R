test_that("define_design creates the correct object structure", {
  # Create design object
  my_design <- define_design(
    sample_size = list(subject = 30),
    between = list(group = c("A", "B")),
    within = list(time = c("t1", "t2"))
  )


  expect_s3_class(my_design, "PowRPriori_design")
  expect_true(is.list(my_design$sample_size))
  expect_true(is.list(my_design$between))
  expect_equal(names(my_design$between), "subject")
  expect_equal(names(my_design$between$subject), "group")
  expect_equal(names(my_design$within), "subject")
  expect_equal(names(my_design$within$subject), "time")
})

test_that("define_design handles hierarchical (cluster) designs", {

  design <- define_design(
    sample_size = list(pupil = 30, class = 10),
    between = list(
      class = list(intervention = c("yes", "no"))
    )
  )

  expect_s3_class(design, "PowRPriori_design")
  expect_equal(names(design$between), "class")
  expect_true(is.list(design$between$class))
  expect_equal(names(design$between$class), "intervention")
})

test_that("define_design handles continuous variables", {

  design <- define_design(
    sample_size = list(subject = 30),
    between = list(age = list(mean = 25, sd = 5))
  )

  expect_true(is.list(design$between$subject$age))
  expect_equal(names(design$between$subject$age), c("mean", "sd"))
  expect_equal(design$between$subject$age$mean, 25)
})

test_that("internal .create_design_matrix creates correct matrices from designs", {
  my_design1 <- define_design(
    sample_size = list(subject = 30),
    between = list(group = c("A", "B"),
                   iq = list(mean = 100, sd = 15)),
    within = list(time = c("t1", "t2"))
  )
  design_matrix1 <- PowRPriori:::.create_design_matrix(my_design1, formula = y ~ group*time + iq + (1|subject))
  expect_equal(nrow(design_matrix1), 60)

  my_design2 <- define_design(
    sample_size = list(subject = 10, class = 5, school = 2),
    between = list(class = list(
                      group = c("A", "B"),
                      iq = list(mean = 100, sd = 15))
                    )
                   ,
    within = list(subject = list(time = c("t1", "t2")))
  )
  design_matrix3 <- PowRPriori:::.create_design_matrix(my_design2, formula = y ~ group*time + iq + (1|school/class/subject))
  expect_equal(nrow(design_matrix3), 200)
})
