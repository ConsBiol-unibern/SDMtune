context("Do Jackknife")

skip_on_cran()

v <- c("bio1", "bio12")
t <- SDMtune:::p
t@data <- t@data[, v]
a <- SDMtune:::bg_model
a@data <- a@data[, v]
m <- train("Maxnet", t, a, fc = "lq")

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

m_cv <- train("Maxnet", t, a, fc = "lq", rep = 2)

test_that("The function returns the correct output", {
  # Metric AUC
  # with_only and return models
  jk <- doJk(m, metric = "auc", variables = v, test = t, with_only = TRUE,
             return_models = TRUE)
  expect_type(jk, "list")
  expect_length(jk, 3)
  expect_named(jk, c("results", "models_without", "models_withonly"))
  expect_named(jk$results,
               c("Variable", "Train_AUC_without", "Train_AUC_withonly",
                 "Test_AUC_without", "Test_AUC_withonly"))
  expect_length(jk$results[, 1], 2)
  expect_equal(class(jk$results), "data.frame")
  expect_length(jk$models_without, 2)
  expect_length(jk$models_withonly, 2)
  expect_named(jk$models_without[[1]]@p@data, "bio12")
  expect_named(jk$models_without[[2]]@p@data, "bio1")
  expect_named(jk$models_withonly[[1]]@p@data, "bio1")
  expect_named(jk$models_withonly[[2]]@p@data, "bio12")
  # with_only = FALSE and return models
  jk <- doJk(m, metric = "auc", variables = v, test = t, with_only = FALSE,
             return_models = TRUE)
  expect_type(jk, "list")
  expect_length(jk, 2)
  expect_named(jk, c("results", "models_without"))
  # with_only and return models = FALSE
  jk <- doJk(m, metric = "auc", variables = v, test = t, with_only = TRUE,
             return_models = FALSE)
  expect_equal(class(jk), "data.frame")
  expect_named(jk, c("Variable", "Train_AUC_without", "Train_AUC_withonly",
                     "Test_AUC_without", "Test_AUC_withonly"))
  expect_length(jk[, 1], 2)
  # with_only = FALSE and return models = FALSE
  jk <- doJk(m, metric = "auc", variables = v, test = t, with_only = FALSE,
             return_models = FALSE)
  expect_equal(class(jk), "data.frame")
  expect_named(jk, c("Variable", "Train_AUC_without", "Test_AUC_without"))
  expect_length(jk[, 1], 2)
  # cross validation model, with_only = FALSE and return models = FALSE
  jk <- doJk(m_cv, metric = "auc", variables = v, test = t, with_only = FALSE,
             return_models = FALSE)
  expect_equal(class(jk), "data.frame")
  expect_named(jk, c("Variable", "Train_AUC_without", "Test_AUC_without"))
  expect_length(jk[, 1], 2)
  # all variables, with_only = FALSE and return models = FALSE
  jk <- doJk(m, metric = "auc", test = t, with_only = FALSE,
             return_models = FALSE)
  expect_equal(class(jk), "data.frame")
  expect_named(jk, c("Variable", "Train_AUC_without", "Test_AUC_without"))
  expect_length(jk[, 1], 2)

  # Metric TSS
  # with_only and return models = FALSE
  jk <- doJk(m, metric = "tss", variables = v, test = t, with_only = TRUE,
             return_models = FALSE)
  expect_equal(class(jk), "data.frame")
  expect_named(jk, c("Variable", "Train_TSS_without", "Train_TSS_withonly",
                     "Test_TSS_without", "Test_TSS_withonly"))
  expect_length(jk[, 1], 2)
  # test, with_only = FALSE and return models = FALSE
  jk <- doJk(m, metric = "tss", variables = v, test = t, with_only = FALSE,
             return_models = FALSE)
  expect_equal(class(jk), "data.frame")
  expect_named(jk, c("Variable", "Train_TSS_without", "Test_TSS_without"))
  expect_length(jk[, 1], 2)

  # Metric AICc
  # with_only and return models = FALSE
  jk <- doJk(m, metric = "aicc", variables = v, with_only = TRUE,
             return_models = FALSE, env = predictors)
  expect_equal(class(jk), "data.frame")
  expect_named(jk, c("Variable", "AICc_without", "AICc_withonly"))
  expect_length(jk[, 1], 2)
  # test, with_only = FALSE and return models = FALSE
  jk <- doJk(m, metric = "aicc", variables = v, with_only = FALSE,
             return_models = FALSE, env = predictors)
  expect_equal(class(jk), "data.frame")
  expect_named(jk, c("Variable", "AICc_without"))
  expect_length(jk[, 1], 2)
})
