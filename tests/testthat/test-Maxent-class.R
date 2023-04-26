test_that("Show method for Maxent class produces the correct output", {
  m <- SDMtune:::bm_maxent@model
  expect_snapshot(m)
})
