context("TSS")

m <- SDMtune:::bm_maxent
m_cv <- SDMtune:::bm_maxent_cv

test_that("The function returns the expected output", {
  expect_true(tss(m) < 1)
  expect_true(is.numeric(tss(m)))
  expect_true(tss(m_cv) < 1)
  expect_true(is.numeric(tss(m_cv)))
})

test_that("The function uses the testing dataset", {
  t <- SDMtune:::p
  t@data <- t@data[1:50, ]
  expect_true(tss(m) != tss(m, test = t))
  expect_true(tss(m_cv) > tss(m_cv, test = TRUE))
})
