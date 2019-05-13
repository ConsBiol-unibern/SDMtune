context("Maxent Feature Classes")

linear <- data.frame(feature = "bio1", lambda = 1.3, min = 0.0, max = 1.0,
                     stringsAsFactors = FALSE)
quadratic <- data.frame(feature = "bio1^2", lambda = 1.3, min = 0.0, max = 1.0,
                        stringsAsFactors = FALSE)
product <- data.frame(feature = "bio1*bio2", lambda = 1.3, min = 0.0, max = 1.0,
                      stringsAsFactors = FALSE)
hinge <- data.frame(feature = "'bio1", lambda = 1.3, min = 0.0, max = 1.0,
                    stringsAsFactors = FALSE)
rev_hinge <- data.frame(feature = "`bio1", lambda = 1.3, min = 0.0, max = 1.0,
                        stringsAsFactors = FALSE)
threshold <- data.frame(feature = "0.2<bio1", lambda = 1.3, min = 0.0, max = 1.0,
                     stringsAsFactors = FALSE)
categ <- data.frame(feature = "biome=1.0", lambda = 1.3, min = 0.0, max = 1.0,
                    stringsAsFactors = FALSE)

test_that("Produce right formula and output for linear feature", {
  expect_equal(.formula_from_lambdas(linear),
               formula("~.linear(bio1, 0, 1) - 1"))
  expect_equal(.linear(350, 300, 400), 0.5)
})

test_that("Produce right formula And output for quadratic feature", {
  expect_equal(.formula_from_lambdas(quadratic),
               formula("~.quadratic(bio1, 0, 1) - 1"))
  expect_equal(.quadratic(350, 300, 400), 1222)
})

test_that("Produce right formula and output for product feature", {
  expect_equal(.formula_from_lambdas(product),
               formula("~.product(bio1, bio2, 0, 1) - 1"))
  expect_equal(.product(350, 50, 300, 400), 172)
})

test_that("Produce right formula and output for hinge feature", {
  expect_equal(.formula_from_lambdas(hinge),
               formula("~.hinge(bio1, 0, 1) - 1"))
  # Variable lower than minimum
  expect_equal(.hinge(290, 300, 400), 0)
  # Variable equal to minimum
  expect_equal(.hinge(300, 300, 400), 0)
  # Variable greater than minimum
  expect_equal(.hinge(350, 300, 400), 0.5)
})

test_that("Produce right formula and output for reverse hinge feature", {
  expect_equal(.formula_from_lambdas(rev_hinge),
               formula("~.revHinge(bio1, 0, 1) - 1"))
  # Variable lower than maximum
  expect_equal(.revHinge(290, 300, 400), 1.1)
  # Variable equal to maximum
  expect_equal(.revHinge(400, 300, 400), 0)
  # Variable greater than maximum
  expect_equal(.revHinge(410, 300, 400), 0)
})

test_that("Produce right formula and output for threshold feature", {
  expect_equal(.formula_from_lambdas(threshold),
               formula("~.threshold(bio1, 0.2) - 1"))
  # Variable lower than threshold
  expect_equal(.threshold(300, 400), 0)
  # Variable equal to threshold
  expect_equal(.threshold(400, 400), 1)
  # Variable greater than threshold
  expect_equal(.threshold(500, 400), 1)
})

test_that("Produce right formula and output for categorical feature", {
  expect_equal(.formula_from_lambdas(categ),
               formula("~.categorical(biome, 1) - 1"))
  # Variable equal to category
  expect_equal(.categorical(1, 1), 1)
  # Variable different from category
  expect_equal(.categorical(2, 1), 0)
})
