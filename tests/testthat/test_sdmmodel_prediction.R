context("SDMtune prediction")

m <- SDMtune:::bm_maxent
p <- SDMtune:::p
files <- list.files(path = paste(system.file(package = "dismo"),
                                 "/ex", sep = ""),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

test_that("The method works with data frames", {
  expect_length(predict(m, p@data, "raw"), nrow(p@data))
})

test_that("The method works with SWD objects", {
  expect_length(predict(m, p, "raw"), nrow(p@data))
})

test_that("The method works with raster stack objects", {
  expect_length(predict(m, predictors, "raw"),
                predictors$bio1@ncols * predictors$bio1@nrows)
})
