context("Do Jackknife")

val <- SDMtune:::p
model <- SDMtune:::bm_maxent
model_cv <- SDMtune:::bm_maxent_cv

test_that("Exceptions are thrown", {

  # .checkArgs function
  # Throws exception if metric is aicc and env is not provided
  expect_error(doJk(model, metric = "aicc"),
               "You must provide the 'env' argument if you want to use the AICc metric!")
  # Throws exception if metric is aicc and model is SDMmodelCV
  expect_error(doJk(model_cv, metric = "aicc"),
               "Metric 'aicc' not allowed with SDMmodelCV objects!")
  # Throws exception if model is SDMmodel metric is not aicc and test is not provided
  expect_error(doJk(model, metric = "auc"),
               "You need to provide a test dataset!")
  })
