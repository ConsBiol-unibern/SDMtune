context("Maxent Prediction")

model <- SDMtune:::bm_maxent
maxent_model <- SDMmodel2MaxEnt(model)
data <- SDMtune:::bg

skip_if(Sys.getenv("R_COVR") == "true",
        message = "Skip Maxent Prediction test with covr!")

test_that("The function predicts correctly", {
  expect_equal(predict(model, data, type = "cloglog"),
               predict(maxent_model, data@data),
               tolerance = 1e-7)
})
