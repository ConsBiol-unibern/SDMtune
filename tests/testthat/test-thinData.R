files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)
set.seed(25)
x <- dismo::randomPoints(predictors, 9000)
c <- thinData(x, predictors)

test_that("The function remove coords where there are NA", {
  expect_equal(nrow(c), 8991)
})

test_that("The function remove duplicated data", {
  c1 <- thinData(rbind(c, c), predictors)
  expect_equal(nrow(c), 8991)
})

test_that("The correct output is returned", {
  expect_equal(class(c), "data.frame")
  expect_named(c, colnames(x))
})
