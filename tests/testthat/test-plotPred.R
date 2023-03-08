map <- terra::rast(matrix(runif(400, 0, 1),
                          nrow = 20,
                          ncol = 20))

test_that("The function raises an error if argument is not a raster", {
  expect_snapshot_error(plotPred(data.frame(a = 1, b = "l")))
})

test_that("The values are correct", {
  p <- plotPred(map)
  expect_true(min(p$data$value) >= 0)
  expect_true(max(p$data$value) <= 1)

  # If hr is TRUE it should use the number of pixel in the raster
  new_map <- terra::rast(matrix(runif(250000, 0, 1),
                                nrow = 500,
                                ncol = 500))
  p <- plotPred(new_map, hr = TRUE)

  expect_equal(nrow(p$data), 250000)
  })

# TODO: Remove with version 2.0.0
test_that("The function raises an error", {
  class(map) <- "RasterLayer"
  expect_snapshot_error(plotPred(map))
})
