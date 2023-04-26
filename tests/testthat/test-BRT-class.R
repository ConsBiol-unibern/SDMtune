skip_on_cran()

test_that("Show method for BRT class produces the correct output", {
  data <- SDMtune:::t
  data@data <- data@data[, 1:4]
  m <- trainBRT(data = data,
                n.trees = 200,
                shrinkage = 0.2,
                bag.fraction = 0.6)
  expect_snapshot(m@model)
})
