skip_on_cran()

test_that("Show method for RF class produces the correct output", {
  data <- SDMtune:::t
  data@data <- data@data[, 1:4]
  m <- train("RF",
             data = data,
             mtry = 2,
             ntree = 200)
  expect_snapshot(m@model)
})
