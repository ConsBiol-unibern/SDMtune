skip_on_cran()

m <- SDMtune:::bm_maxent
m1 <- SDMtune:::bm_maxnet
train <- SDMtune:::t
train@data <- train@data[train@pa == 1, ]
train@coords <- train@coords[train@pa == 1, ]
train@pa <- train@pa[train@pa == 1]

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)
e = terra::ext(c(-77, -60, -56, -15))

folder <- tempfile("SDMtune")
dir.create(folder)

# TODO: Remove with version 2.0.0
predictors_raster <- predictors
class(predictors_raster) <- "Raster"
e_raster = e
class(e_raster) <- "Extent"

test_that("The method works with data frames", {
  p <- predict(m,
               data = train@data,
               type = "raw",
               clamp = FALSE)

  expect_length(p, nrow(train@data))
  expect_vector(p)
})

test_that("The method works with SWD objects", {
  p <- predict(m1,
               data = train,
               type = "logistic")

  expect_length(p, nrow(train@data))
  expect_vector(p)
})

test_that("The method works with raster objects", {
  p <- predict(m,
               data = predictors,
               type = "raw",
               filename = file.path(folder, "spam.tif"))

  withr::defer(unlink(folder))

  expect_equal(terra::ncell(p), terra::ncell(predictors))
  expect_s4_class(p, "SpatRaster")
  expect_true(file.exists(file.path(folder, "spam.tif")))
})

test_that("Rasters are cropped to extent", {
  p <- predict(m,
               data = predictors,
               type = "raw",
               extent = e)

  expect_equal(as.vector(terra::ext(p)), as.vector(e))
})

test_that("The function raises errors", {
  expect_snapshot_error(predict(m,
                                data = "spam",
                                type = "raw"))

  expect_snapshot_error(predict(m,
                                data = predictors,
                                type = "raw",
                                extent = "spam"))

  expect_snapshot_error(predict(m,
                                data = predictors,
                                type = "raw",
                                filename = "spam"))
})

# TODO: Remove with version 2.0.0
test_that("The function raises errors", {
  expect_snapshot_error(predict(m,
                                data = predictors_raster,
                                type = "raw"))

  expect_snapshot_error(predict(m,
                                data = predictors,
                                type = "raw",
                                extent = e_raster))

  expect_snapshot_error(predict(m,
                                data = predictors,
                                type = "raw",
                                extent = "spam"))
})
