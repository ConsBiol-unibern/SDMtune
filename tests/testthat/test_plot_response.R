context("Plot Response")

skip_on_cran()

m <- SDMtune:::bm_maxnet
m_cv <- SDMtune:::bm_maxnet_cv

test_that("Error are raised", {
  expect_error(plotResponse(m, "biowhat", "cloglog"),
               "biowhat is not used to train the model!")
})

test_that("Labels and output are correct for SDMmodel objects", {
  p <- plotResponse(m, "bio1", "cloglog", rug = TRUE, marginal = TRUE)
  expect_length(p$layers, 3)  # line and two rugs
  expect_equal(p$labels$x, "bio1")
  expect_equal(p$labels$y, "cloglog output")
  expect_true(min(p$data$y) >= 0)
  expect_true(max(p$data$y) <= 1)
  p <- plotResponse(m, "bio1", "logistic", marginal = FALSE)
  expect_length(p$layers, 1)  # only line
  expect_equal(p$labels$x, "bio1")
  expect_equal(p$labels$y, "logistic output")
  expect_true(min(p$data$y) >= 0)
  expect_true(max(p$data$y) <= 1)
})

test_that("Labels and output are correct for SDMmodelCV objects", {
  p <- plotResponse(m_cv, "bio1", "exponential", rug = TRUE, marginal = TRUE)
  expect_length(p$layers, 4)  # line, ribbon and two rugs
  expect_equal(p$labels$x, "bio1")
  expect_equal(p$labels$y, "exponential output")
})
