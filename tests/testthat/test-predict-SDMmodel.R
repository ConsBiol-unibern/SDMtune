skip_on_cran()

m <- SDMtune:::bm_maxent
m1 <- SDMtune:::bm_maxnet
train <- SDMtune:::t
train@data <- train@data[train@pa == 1, ]
train@coords <- train@coords[train@pa == 1, ]
train@pa <- train@pa[train@pa == 1]
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

test_that("The method works with data frames", {
  expect_length(predict(m, train@data, "raw", clamp = FALSE), nrow(train@data))
})

test_that("The method works with SWD objects", {
  expect_length(predict(m1, train, "logistic"), nrow(train@data))
})

test_that("The method works with raster stack objects", {
  expect_length(predict(m, predictors, "raw"),
                predictors$bio1@ncols * predictors$bio1@nrows)
})

test_that("The method works with raster stack objects and parallel", {
  expect_length(predict(m, predictors, "raw", parallel = TRUE),
                predictors$bio1@ncols * predictors$bio1@nrows)
})
