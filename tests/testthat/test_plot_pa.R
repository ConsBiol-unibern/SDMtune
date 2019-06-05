context("Plot Presence Absence")

files <- list.files(path = paste(system.file(package = "dismo"),
                                 "/ex", sep = ""),
                    pattern = "grd", full.names = TRUE)
map <- raster::raster(files[1])

test_that("The function raises an error if argument is not raster", {
  expect_error(plotPA(data.frame(a = 1, b = "l")))
})

test_that("The values are correct", {
  p <- plotPA(map, th = 100)
  expect_equal(p$plot_env$maxpixels, 50000)
  expect_equal(unique(p$data$value), c(NA, TRUE, FALSE))
  # If hr is TRUE it should use the number of pixel in the raster
  p <- plotPA(map, th = 100, hr = TRUE)
  expect_equal(p$plot_env$maxpixels, 35712)
})
