test_that("Raise the exception", {
  expect_snapshot(maxentTh(SDMtune:::bm_maxnet), error = TRUE)
})

test_that("Create the correct output", {
  expect_named(maxentTh(SDMtune:::bm_maxent))
  expect_s3_class(maxentTh(SDMtune:::bm_maxent), "data.frame")
})
