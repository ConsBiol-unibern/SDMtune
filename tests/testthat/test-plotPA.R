map <- terra::rast(matrix(runif(400, 0, 1),
                          nrow = 20,
                          ncol = 20))
file <- tempfile(fileext = ".tif")

test_that("The values are correct and the file is saved", {
  withr::defer(unlink(file))
  p <- plotPA(map,
              th = .8,
              filename = file)

  expect_setequal(p$data$value, c(FALSE, TRUE))
  expect_true(file.exists(file))

  # If hr is TRUE it should use the number of pixel in the raster
  new_map <- terra::rast(matrix(runif(250000, 0, 1),
                                nrow = 500,
                                ncol = 500))
  p <- plotPA(new_map,
              th = 0.8,
              hr = TRUE)

  expect_equal(nrow(p$data), 250000)
})

test_that("The function raises errors", {
  expect_snapshot_error(plotPA(data.frame(a = 1, b = "l"),
                               th = 0.8))

  expect_snapshot_error(plotPA(map,
                               th = .8,
                               filename = "spam"))
})

test_that("The function warns and raises errors", {
  # TODO: Remove with version 2.0.0
  x <- integer(1)
  class(x) <- "Raster"
  expect_snapshot_error(plotPA(x,
                        th = .8))
})
