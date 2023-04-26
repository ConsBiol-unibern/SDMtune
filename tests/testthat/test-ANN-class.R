skip_on_cran()

test_that("Show method for ANN class produces the correct output", {
  data <- SDMtune:::t
  data@data <- data@data[, 1:4]
  m <- train("ANN",
             data = data,
             size = 10)
  expect_snapshot(m@model)
})
