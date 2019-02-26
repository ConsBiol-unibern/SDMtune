context("Tune hyperparameters")

val <- SDMtune:::t
bg <- SDMtune:::bg
model <- SDMtune:::bm_maxent
model_cv <- SDMtune:::bm_maxent_cv

test_that("Exceptions are thrown", {

  # .checkArgs function
  h <- list("fc" = c("l", "lq", "lqp"), "reg" = seq(.2, 2., .2), "a" = 10000)
  # Throws exception if metric is aicc and env is not provided
  expect_error(.checkArgs(model, h, metric = "aicc"),
               "You must provide the 'env' argument if you want to use the AICc metric!")
  # Throws exception if metric is aicc and model is SDMmodelCV
  expect_error(.checkArgs(model_cv, h, metric = "aicc"),
               "Metric 'aicc' not allowed with SDMmodelCV objects!")
  # Throws exception if model is SDMmodel metric is not aicc and test is not provided
  expect_error(.checkArgs(model, h, metric = "auc"),
               "You need to provide a test dataset!")
  # Throws exception if hypers includes 'a' and bg4test is not provided
  expect_error(.checkArgs(model, h, metric = "auc", test = t),
               "bg4test must be provided to tune background locations!")
  # Throws exception if max hypers 'a' > than nrow bg4test
  expect_error(.checkArgs(model, h, metric = "auc", test = t,
                                    bg4test = bg),
               "Maximum number of 'a' hyperparameter cannot be more than 9000!")
  # Doesn't throw exception if 'a' is not in hypers and bg4test is not provided
  h$a <- NULL
  expect_error(.checkArgs(model, h, metric = "auc", test = val), NA)
  # Throws exception if provided hypers are not tunable
  h <- list("fc" = c("l", "lq", "lqp"), "lambda" = c(500, 600))
  expect_error(.checkArgs(model, h, "auc", t),
               "lambda non included in tunable hyperparameters")
  h <- list("beta" = c(1, 2, 3), "lambda" = c(500, 600))
  expect_error(.checkArgs(model, h, "auc", t),
               paste("beta non included in tunable hyperparameters,",
                     "lambda non included in tunable hyperparameters"))

  # .check_optimise_hypers function
  # Throws exception if less than two hypers have more than two values
  h <- list("fc" = c("l", "lq"), "reg" = c(.2, .4), "a" = 10000)
  grid <- .get_hypers_grid(model, h)
  expect_error(.check_optimise_args(h, grid, pop = 5),
               "One hyperparameter in hypers should have more than two values to allow crossover!")
  # Throws exception if there are less than two hypers to be tuned
  h <- list("fc" = c("l", "lq", "lqp"))
  expect_error(.check_optimise_args(h, pop = 5),
               "You must provide at least two hyperparameters to be tuned! Use gridSearch to tune only one parameter.")
  # Throws exception if possible random combinations < pop
  h <- list("fc" = c("l", "lq", "lqp"), "reg" = c(.2, .4), "a" = 10000)
  grid <- .get_hypers_grid(model, h)
  expect_error(.check_optimise_args(h, grid, pop = 7),
               "Number of possible random models is lewer than population size, add more values to the 'hyper' argument!")
  # Throws exception if possible random combinations < pop
  expect_error(.check_optimise_args(h, grid, pop = 6),
               "Number of possible random models is the same than population size. Use gridSearch function!")
})
