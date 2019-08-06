skip_on_cran()

test_that("Show method for BRT class produces the correct output", {
  data <- SDMtune:::t
  data@data <- data@data[, 1:4]
  m <- trainBRT(data = data, ntree = 200, lr = 0.2,
                bag.fraction = 0.6)@model
  expect_output(print(m), "Class            : BRT", fixed = TRUE)
  expect_output(print(m), "distribution     : bernoulli", fixed = TRUE)
  expect_output(print(m), "ntree            : 200", fixed = TRUE)
  expect_output(print(m), "lr               : 0.2", fixed = TRUE)
  expect_output(print(m), "bag.fraction     : 0.6", fixed = TRUE)
})

