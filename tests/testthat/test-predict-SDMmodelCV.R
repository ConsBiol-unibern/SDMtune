skip_on_cran()

m <- SDMtune:::bm_maxent_cv
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
               type = "raw")

  expect_length(p, nrow(train@data))
  expect_vector(p)
})

test_that("The method works with SWD objects", {
  p <- predict(m,
               data = train,
               type = "raw")

  expect_length(p, nrow(train@data))
  expect_vector(p)
})

test_that("The method works with raster objects", {
  p <- predict(m,
               data = predictors,
               type = "raw")

  expect_equal(terra::ncell(p), terra::ncell(predictors))
  expect_s4_class(p, "SpatRaster")
})

test_that("The output is the function applied to the k predictions", {
  train@data <- train@data[1:3, ]

  p <- predict(m,
               data = train@data,
               fun = c("mean", "sd", "min"),
               type = "raw")

  expect_equal(class(p), "list")
  expect_vector(p, size = 3)
  expect_named(p, c("mean", "sd", "min"))

  # mean
  preds <- matrix(nrow = 3, ncol = 4)

  for (i in 1:4) {
    preds[, i] <- predict(m@models[[i]],
                          data = train@data,
                          type = "raw")
  }

  for (i in 1:3) {
    expect_equal(p$mean[i], mean(preds[i, ]))
  }

  # sd
  preds <- matrix(nrow = 3, ncol = 4)

  for (i in 1:4) {
    preds[, i] <- predict(m@models[[i]],
                          data = train@data,
                          type = "raw")
  }

  for (i in 1:3) {
    expect_equal(p$sd[i], sd(preds[i, ]))
  }

  # min
  preds <- matrix(nrow = 3, ncol = 4)

  for (i in 1:4) {
    preds[, i] <- predict(m@models[[i]],
                          data = train@data,
                          type = "raw")
  }

  for (i in 1:3) {
    expect_equal(p$min[i], min(preds[i, ]))
  }
})

test_that("The function works with raster data and multiple functions", {

  funs <- c("mean", "sd", "min")

  p <- predict(m,
               data = predictors,
               fun = funs,
               type = "raw",
               filename = paste0(file.path(folder, "pred"), ".tif"))

  withr::defer(unlink(folder))

  expect_equal(class(p), "list")
  expect_vector(p, size = 3)
  expect_named(p, c("mean", "sd", "min"))

  # check that files are created
  for (i in 1:3) {
    expect_true(file.exists(paste0(file.path(folder, "pred_"), funs[i],
                                   ".tif")))
    expect_s4_class(p[[i]], "SpatRaster")
  }

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
                                data = train@data,
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
                                extent = "span"))
})
