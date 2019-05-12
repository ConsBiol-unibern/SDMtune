context("Maxent Prediction")

model <- SDMtune:::bm_maxent
maxent_model <- SDMmodel2MaxEnt(model)
data <- SDMtune:::p

skip_if(Sys.getenv("R_COVR") == "true",
        message = "Skip Maxent Prediction test with covr!")

test_that("The function predicts cloglog correctly", {
  expect_equal(predict(model, data, type = "cloglog"),
               predict(maxent_model, data@data),
               tolerance = 1e-7)
})

test_that("The function predicts logistic correctly", {
  expect_equal(predict(model, data, type = "logistic"),
               predict(maxent_model, data@data, args = "outputformat=logistic"),
               tolerance = 1e-7)
})

test_that("The function predicts raw correctly", {
  expect_equal(predict(model, data, type = "raw"),
               predict(maxent_model, data@data, args = "outputformat=raw"),
               tolerance = 1e-7)
})

model <- train("Maxent", SDMtune:::p, SDMtune:::bg_model, fc = "l")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc l correctly", {
  expect_equal(predict(model, data, type = "cloglog"),
               predict(maxent_model, data@data),
               tolerance = 1e-7)
})

model <- train("Maxent", SDMtune:::p, SDMtune:::bg_model, fc = "q")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc q correctly", {
  expect_equal(predict(model, data, type = "cloglog"),
               predict(maxent_model, data@data),
               tolerance = 1e-7)
})

model <- train("Maxent", SDMtune:::p, SDMtune:::bg_model, fc = "p")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc p correctly", {
  expect_equal(predict(model, data, type = "cloglog"),
               predict(maxent_model, data@data),
               tolerance = 1e-7)
})

model <- train("Maxent", SDMtune:::p, SDMtune:::bg_model, fc = "h")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc h correctly", {
  expect_equal(predict(model, data, type = "cloglog"),
               predict(maxent_model, data@data),
               tolerance = 1e-7)
})

model <- train("Maxent", SDMtune:::p, SDMtune:::bg_model, fc = "t")
maxent_model <- SDMmodel2MaxEnt(model)

test_that("The function predicts fc t correctly", {
  expect_equal(predict(model, data, type = "cloglog"),
               predict(maxent_model, data@data),
               tolerance = 1e-7)
})
