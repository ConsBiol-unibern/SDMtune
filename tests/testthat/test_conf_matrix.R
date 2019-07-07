context("Confusion matrix")

skip_on_cran()

cm <- confMatrix(SDMtune:::bm_maxnet, th = 0.4)

test_that("There is the correct number of thresholds when passing th", {
  expect_length(cm$th, 1)
})

test_that("The output as the correct column names", {
  expect_named(cm, c("th", "tp", "fp", "fn", "tn"))
})

test_that("The output is correct", {
  # Threshold is correct
  expect_equal(cm$th, 0.4)
  # Sum of tp, fp, fn, tn is equal to sum of presence and background locations
  expect_equal(sum(cm[1, 2:5]), sum(nrow(SDMtune:::bm_maxnet@p@data),
                                    nrow(SDMtune:::bm_maxnet@a@data)))
  # Sum of tp and fn is equal to number of presence locations
  expect_equal(sum(cm$tp, cm$fn), nrow(SDMtune:::bm_maxnet@p@data))
  # Sum of tn and fp is equal to number of background locations
  expect_equal(sum(cm$tn, cm$fp), nrow(SDMtune:::bm_maxnet@a@data))
  # Correct output with test argument
  # 5023 is the number of unique prediction values plus 0 and 1
  expect_equal(nrow(confMatrix(SDMtune:::bm_maxnet,
                               test = getSubsample(SDMtune:::p, 50, seed = 2))),
               5023)
})

test_that("The thresholds start with 0 and end with 1 when th is not passed", {
  cm <- confMatrix(SDMtune:::bm_maxnet)
  expect_equal(cm$th[1], 0)
  expect_equal(cm$th[nrow(cm)], 1)
})
