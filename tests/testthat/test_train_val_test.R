context("Train Validation Test")

test_that("Exception are raised", {
  expect_error(trainValTest(SDMtune:::p@data, .2), "x must be an SWD object!")
})

test_that("Correct output is produced", {
  # For train/test split
  expect_length(trainValTest(SDMtune:::p, 0.3, seed = 25), 2)
  # For train/validation/test split
  expect_length(trainValTest(SDMtune:::p, test = 0.3, val = 0.2, seed = 25), 3)
  # Correct output type
  expect_type(trainValTest(SDMtune:::p, 0.3, seed = 25), "list")
  expect_s4_class(trainValTest(SDMtune:::p, 0.3, seed = 25)[[1]], "SWD")
})
