skip_on_cran()

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- terra::rast(files)
set.seed(25)
x <- terra::spatSample(predictors,
                       size = 9000,
                       method = "random",
                       xy = TRUE,
                       values = FALSE)

test_that("The function remove coords where there are NA (ex with matrix)", {
  c <- thinData(x,
                env = predictors,
                verbose = FALSE,
                progress = FALSE)

  expect_true(nrow(c) < 9000)
  expect_true(inherits(c, "matrix"))
  expect_equal(colnames(c), colnames(x))
})

test_that("The function remove duplicated data (ex with dataframe)", {
  c <- thinData(as.data.frame(rbind(x, x)),
                env = predictors,
                verbose = FALSE,
                progress = FALSE)

  expect_true(nrow(c) < 9000)
  expect_true(inherits(c, "data.frame"))
  expect_equal(colnames(c), colnames(x))
})

test_that("The function works with custom dataframe", {
  df <- data.frame(A = x[, "x"], B = x[, "y"], t = rep("a", nrow(x)))
  c <- thinData(df,
                env = predictors,
                x = "A",
                y = "B",
                verbose = FALSE,
                progress = FALSE)

  expect_true(nrow(c) < 9000)
  expect_true(inherits(c, "data.frame"))
  expect_equal(colnames(c), colnames(df))
})

test_that("The function writes messages", {
  # Only duplicates
  x <- terra::spatSample(predictors,
                         size = 9000,
                         method = "random",
                         na.rm = TRUE,
                         xy = TRUE,
                         values = FALSE)

  expect_snapshot(c <- thinData(rbind(x, x),
                                env = predictors,
                                progress = FALSE))

  # Only NAs
  x <- terra::spatSample(predictors,
                         size = 9000,
                         method = "random",
                         xy = TRUE,
                         values = FALSE)

  expect_snapshot(c <- thinData(x,
                                env = predictors,
                                progress = FALSE))

  # Both, duplicates and NAs
  x <- terra::spatSample(predictors,
                         size = 9000,
                         method = "random",
                         xy = TRUE,
                         values = FALSE)

  expect_snapshot(c <- thinData(rbind(x, x),
                                env = predictors,
                                progress = FALSE))
})

test_that("The function raises errors", {
  expect_snapshot_error(thinData(x,
                                 env = predictors,
                                 x = "A",
                                 verbose = FALSE,
                                 progress = FALSE))

  expect_snapshot_error(thinData(x,
                                 env = predictors,
                                 y = "B",
                                 verbose = FALSE,
                                 progress = FALSE))

  expect_snapshot_error(thinData(x,
                                 env = "spam"))
})

# TODO: Remove with version 2.0.0
test_that("The function warns if a raster object is used", {
  expect_snapshot_warning(thinData(x,
                                   env = raster::stack(files),
                                   verbose = FALSE,
                                   progress = FALSE))
})
