context("Plot Response")

model <- SDMtune:::bm_maxnet

test_that("Error are raised", {
  expect_error(plotResponse(model, "biowhat", "cloglog"),
               "biowhat is not used to train the model!")
})

test_that("An invisible object of class ggplot is returned", {
          expect_equal(class(plotResponse(m, "bio1", "cloglog"))[2], "ggplot")
})
