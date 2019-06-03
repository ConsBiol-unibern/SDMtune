context("AICc")

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
env <- raster::stack(files)
p <- SDMtune:::p
# Reduce observation to simulate k > n
p@data <- p@data[1:15, ]
p@coords <- p@coords[1:15, ]
bg <- SDMtune:::bg_model
m <- train("Maxnet", p = p, a = bg, fc = "h")

test_that("NA is returned if k > obs", {
  expect_equal(aicc(m, env), NA)
})

test_that("The correct output is produced", {
  expect_type(aicc(SDMtune:::bm_maxent, env), "double")
})
