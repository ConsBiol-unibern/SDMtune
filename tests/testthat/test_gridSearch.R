context("Grid Search")

val <- SDMtune:::t
model <- SDMtune:::bm_maxent
model_cv <- SDMtune:::bm_maxent_cv
h <- list("fc" = c("l", "lq", "lqp"), "reg" = seq(.2, 2., .2),
          "a" = 10000)

test_that("Exceptions are thrown", {
  # Throws exception if metric is aicc and env is not provided
  expect_that(.checkGridSearchArgs(model, h, metric = "aicc"),
              throws_error("You must provide the 'env' argument if you want to use the AICc metric!"))
  # Throws exception if metric is aicc and model is SDMmodelCV
  expect_that(.checkGridSearchArgs(model_cv, h, metric = "aicc"),
              throws_error("Metric 'aicc' not allowed with SDMmodelCV objects!"))
  # Throws exception if model is SDMmodel metric is not aicc and test is not provided
  expect_that(.checkGridSearchArgs(model, h, metric = "auc"),
              throws_error("You need to provide a test dataset!"))
  # Throws exception if hypers includes 'a' and bg4test is not provided
  expect_that(.checkGridSearchArgs(model, h, "auc", t),
              throws_error("bg4test must be provided to tune background locations!"))
  # Throws exception if max hypers 'a' > than nrow bg4test
  expect_that(.checkGridSearchArgs(model, h, "auc", t, bg4test = bg),
              throws_error("Maximum number of 'a' hyperparameter cannot be more than 9000!"))
})
