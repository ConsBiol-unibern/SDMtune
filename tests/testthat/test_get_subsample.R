context("Get subsamples")

s <- getSubsample(SDMtune:::bg_model, 3000, seed = 25)

test_that("The function rises errors", {
  expect_error(getSubsample(SDMtune:::bg_model, 6000),
               "6000 is bigger than dataset observations!")
  expect_error(getSubsample(SDMtune:::bg_model@data, 3000),
               "Data set must be an SWD object!")
})

test_that("The output is correct", {
  # The output is an SWD object
  expect_s4_class(s, "SWD")
  # The subsample has the correct size
  expect_equal(nrow(s@data), 3000)
  expect_equal(nrow(s@coords), 3000)
  # The data and coords dataframes have the correct rowname
  expect_equal(rownames(s@data), as.character(1:3000))
  expect_equal(rownames(s@coords), as.character(1:3000))
  # The species is the same of the original SWD object
  expect_equal(s@species, SDMtune:::bg_model@species)
})
