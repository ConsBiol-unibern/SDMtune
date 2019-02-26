context("Grid Search")

val <- SDMtune:::t
bg <- SDMtune:::bg
model <- SDMtune:::bm_maxent
model_cv <- SDMtune:::bm_maxent_cv

test_that("Exceptions are thrown", {
  h <- list("fc" = c("l", "lq", "lqp"), "reg" = seq(.2, 2., .2), "a" = 10000)
  # Throws exception if metric is aicc and env is not provided
  expect_error(.checkGridSearchArgs(model, h, metric = "aicc"),
               "You must provide the 'env' argument if you want to use the AICc metric!")
  # Throws exception if metric is aicc and model is SDMmodelCV
  expect_error(.checkGridSearchArgs(model_cv, h, metric = "aicc"),
               "Metric 'aicc' not allowed with SDMmodelCV objects!")
  # Throws exception if model is SDMmodel metric is not aicc and test is not provided
  expect_error(.checkGridSearchArgs(model, h, metric = "auc"),
               "You need to provide a test dataset!")
  # Throws exception if hypers includes 'a' and bg4test is not provided
  expect_error(.checkGridSearchArgs(model, h, metric = "auc", test = t),
               "bg4test must be provided to tune background locations!")
  # Throws exception if max hypers 'a' > than nrow bg4test
  expect_error(.checkGridSearchArgs(model, h, metric = "auc", test = t,
                                    bg4test = bg),
               "Maximum number of 'a' hyperparameter cannot be more than 9000!")
  # Doesn't throw exception if 'a' is not in hypers and bg4test is not provided
  h$a <- NULL
  expect_error(.checkGridSearchArgs(model, h, metric = "auc", test = val), NA)
  # Throws exception if provided hypers are not tunable
  h <- list("fc" = c("l", "lq", "lqp"), "lambda" = c(500, 600))
  expect_error(.checkGridSearchArgs(model, h, "auc", t),
               "lambda non included in tunable hyperparameters")
  h <- list("beta" = c(1, 2, 3), "lambda" = c(500, 600))
  expect_error(.checkGridSearchArgs(model, h, "auc", t),
               paste("beta non included in tunable hyperparameters,",
                     "lambda non included in tunable hyperparameters"))
})
