context("Plot Response")

model <- SDMtune:::bm_maxnet

test_that("Error are raised", {
  expect_error(plotResponse(model, "biowhat", "cloglog"),
               "biowhat is not used to train the model!")
})
