skip_on_cran()

m <- SDMtune:::bm_maxent
m1 <- SDMtune:::bm_maxnet
train <- SDMtune:::t
train@data <- train@data[train@pa == 1, ]
train@coords <- train@coords[train@pa == 1, ]
train@pa <- train@pa[train@pa == 1]
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- terra::rast(files)
e = terra::ext(c(-77, -60, -56, -15))

# TODO: Remove with version 2.0.0
predictors_raster <- raster::stack(files)
e_raster = raster::extent(c(-77, -60, -56, -15))

test_that("The method works with data frames", {
  p <- predict(m, train@data, "raw", clamp = FALSE)
  expect_length(p, nrow(train@data))
  expect_vector(p)
})

test_that("The method works with SWD objects", {
  p <- predict(m1, train, "logistic")
  expect_length(p, nrow(train@data))
  expect_vector(p)
})

test_that("The method works with raster stack objects", {
  p <- predict(m, predictors, "raw")
  expect_equal(terra::ncell(p), terra::ncell(predictors))
  expect_s4_class(p, "SpatRaster")
})

test_that("Rasters are cropped to extent", {
  p <- predict(m, predictors, "raw", extent = e)
  expect_equal(as.vector(terra::ext(p)), as.vector(e))
})

test_that("The function raises errors", {
  expect_snapshot_error(predict(m, "spam", "raw"))
  expect_snapshot_error(predict(m, predictors, "raw", extent = "spam"))
})

# TODO: Remove with version 2.0.0
test_that("The function warns", {
  expect_snapshot_warning(predict(m, predictors_raster, "raw"))
  expect_snapshot_warning(predict(m, predictors, "raw", extent = e_raster))
})
