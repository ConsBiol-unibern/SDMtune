context("SDMtune prediction")

skip_on_cran()

m <- SDMtune:::bm_maxent
p <- SDMtune:::p
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

test_that("The method works with data frames", {
  expect_length(predict(m, p@data, "raw"), nrow(p@data))
})

test_that("The method works with SWD objects", {
  expect_length(predict(m, p, "logistic"), nrow(p@data))
})

test_that("The method works with raster stack objects", {
  expect_length(predict(m, predictors, "raw"),
                predictors$bio1@ncols * predictors$bio1@nrows)
})

test_that("The method works with raster stack objects and parallel", {
  expect_length(predict(m, predictors, "raw", parallel = TRUE),
                predictors$bio1@ncols * predictors$bio1@nrows)
})
