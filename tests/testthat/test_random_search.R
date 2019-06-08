context("Random Search")

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)
h <- list(fc = c("l", "q", "p"), reg = 1:3, a = c(2000, 3000, 4000))
o <- randomSearch(SDMtune:::bm_maxnet, h, "aicc", bg4test = SDMtune:::bg_model,
                  pop = 3, env = predictors, seed = 25)

test_that("randomSearch produces the expected output", {
  expect_s4_class(o, "SDMtune")
  expect_s4_class(o@models[[1]], "SDMmodel")
  expect_s3_class(o@results, "data.frame")
  expect_named(o@results, c("a", "fc", "reg", "AICc", "delta_AICc"))
  expect_length(o@results$a, 3)
  expect_length(o@models, 3)
  expect_equal(o@results$a, c(3000, 3000, 3000))
  expect_equal(o@results$fc, c("q", "q", "q"))
  expect_equal(o@results$reg, c(1, 2, 3))
})
