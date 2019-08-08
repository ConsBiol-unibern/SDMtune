skip_on_cran()

train <- SDMtune:::t
folds <- randomFolds(train, k = 2)

# TODO Remove this with next release
test_that("Error and warnings are raised", {
  expect_error(train("Maxnet", p = train, a = train, rep = 2),
               "Argument \"p\" and \"a\" are deprecated, use \"data\" instead.")
  expect_warning(train("Maxnet", data = train, rep = 2, fc = "l"),
                 "is deprecated")
  expect_warning(train("Maxnet", data = train, fc = "l", seed = 25),
                 "is deprecated")
})

test_that("Cross validation is executed", {
  cv <- train("Maxnet", data = train, folds = folds, fc = "l")
  expect_s4_class(cv, "SDMmodelCV")
  expect_length(cv@models, 2)
  expect_equal(cv@data, train)
  expect_length(cv@folds, 2)
  expect_equal(ncol(cv@folds[[1]]), 2)
  expect_equal(ncol(cv@folds[[2]]), 2)
})

test_that("Train without ross validation creates the correct output", {
  m <- train("Maxnet", data = train, fc = "l")
  expect_s4_class(m, "SDMmodel")
})
