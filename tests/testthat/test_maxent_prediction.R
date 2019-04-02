context("Maxent Predicion")

model <- SDMtune:::bm_maxent
maxent_model <- SDMmodel2MaxEnt(model)
data <- SDMtune:::bg

test_that("The function predicts correctly", {
  expect_equal(predict(model, data, type = "cloglog"),
               predict(maxent_model, data@data),
               tolerance = 1e-7)
})
