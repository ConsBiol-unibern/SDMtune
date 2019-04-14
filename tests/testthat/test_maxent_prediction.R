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
