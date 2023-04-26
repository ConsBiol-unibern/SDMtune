test_that("Show method for Maxnet class produces the correct output", {
  m <- SDMtune:::bm_maxnet@model
  expect_snapshot(m)
})
