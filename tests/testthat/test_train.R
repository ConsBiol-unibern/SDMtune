context("Train")

test_that("Cross validation is executed", {
  cv <- train("Maxnet", SDMtune:::p, SDMtune:::bg_model, rep = 2, seed = 25)
  expect_s4_class(cv, "SDMmodelCV")
  expect_length(cv@models, 2)
  expect_equal(cv@p, SDMtune:::p)
  expect_equal(cv@a, SDMtune:::bg_model)
  expect_equal(sort(unique(cv@folds)), c(1, 2))
})
