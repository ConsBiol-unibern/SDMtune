context("AICc")

files <- list.files(path = paste(system.file(package = "dismo"),
                                 "/ex", sep = ""),
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
