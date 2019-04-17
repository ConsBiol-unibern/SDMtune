context("Variable Selection")

val <- SDMtune:::p
bg <- SDMtune:::bg
model <- SDMtune:::bm_maxent
model_cv <- SDMtune:::bm_maxent_cv

test_that("Exceptions are thrown", {

  # .checkArgs function
  # Throws exception if metric is aicc and env is not provided
  expect_error(varSel(model, metric = "aicc", bg4cor = bg),
               "You must provide the 'env' argument if you want to use the AICc metric!")
  # Throws exception if metric is aicc and model is SDMmodelCV
  expect_error(varSel(model_cv, metric = "aicc", bg4cor = bg),
               "Metric 'aicc' not allowed with SDMmodelCV objects!")
  # Throws exception if model is SDMmodel metric is not aicc and test is not provided
  expect_error(varSel(model, metric = "auc", bg4cor = bg),
               "You need to provide a test dataset!")

  expect_error(varSel(SDMtune:::bm_maxnet, metric = "auc", bg4cor = bg,
                      test = val, use_pc = TRUE),
               "Percent contribution cannot be used with model of method Maxnet")
})
