test_that("Show method for SWD class produces the correct output", {
  data <- SDMtune:::t
  expect_snapshot(data)
  data@data <- data@data[, 1:3]
  expect_snapshot(data)
  data@data <- SDMtune:::t@data[, "biome", drop = FALSE]
  expect_snapshot(data)
})
