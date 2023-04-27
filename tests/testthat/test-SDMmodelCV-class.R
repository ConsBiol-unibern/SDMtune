test_that("Show method for SDMmodelCV class produces the correct output", {
  m <- SDMtune:::bm_maxent_cv
  expect_snapshot(m)
  m@data@data <- SDMtune:::t@data[, 1:3]
  expect_snapshot(m)
  m@data@data <- SDMtune:::t@data[, "biome", drop = FALSE]
  expect_snapshot(m)
  m <- SDMtune:::bm_maxnet_cv
  expect_snapshot(m)
})
