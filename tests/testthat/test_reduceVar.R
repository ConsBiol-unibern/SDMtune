context("Reduce Variables")

val <- SDMtune:::p
model <- SDMtune:::bm_maxent
model_cv <- SDMtune:::bm_maxent_cv

test_that("Exceptions are thrown", {

  # .checkArgs function
  # Throws exception if metric is aicc and env is not provided
  expect_error(reduceVar(model, th = 2, metric = "aicc", use_jk = TRUE),
               "You must provide the 'env' argument if you want to use the AICc metric!")
  # Throws exception if metric is aicc and model is SDMmodelCV
  expect_error(reduceVar(model_cv, th = 2, metric = "aicc", use_jk = TRUE),
               "Metric 'aicc' not allowed with SDMmodelCV objects!")
  # Throws exception if model is SDMmodel metric is not aicc and test is not provided
  expect_error(reduceVar(model, th = 2, metric = "auc", use_jk = TRUE),
               "You need to provide a test dataset!")

  expect_error(reduceVar(SDMtune:::bm_maxnet, th = 2, metric = "auc",
                         test = val, use_pc = TRUE),
               "Percent contribution cannot be used with model of method Maxnet")
})
