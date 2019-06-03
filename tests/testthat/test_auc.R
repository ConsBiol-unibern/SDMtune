context("AUC")

m <- SDMtune:::bm_maxent
m_cv <- SDMtune:::bm_maxent_cv

test_that("The function returns the expected output", {
  expect_true(auc(m) < 1)
  expect_true(is.numeric(auc(m)))
  expect_true(auc(m_cv) < 1)
  expect_true(is.numeric(auc(m_cv)))
  # tests for Maxnet model
  expect_true(auc(SDMtune:::bm_maxnet) < 1)
  expect_true(is.numeric(auc(SDMtune:::bm_maxnet)))
})

test_that("The function uses the testing dataset", {
  t <- SDMtune:::p
  t@data <- t@data[1:50, ]
  expect_true(auc(m) != auc(m, test = t))
  expect_true(auc(m_cv) > auc(m_cv, test = TRUE))
})

test_that("The function uses the absence dataset", {
  a <- SDMtune:::bg_model
  a@data <- a@data[1:3000, ]
  expect_true(auc(m) != auc(m, a = a))
})
