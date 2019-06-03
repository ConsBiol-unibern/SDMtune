context("Prepare SWD")

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
env <- raster::stack(files)

test_that("Warning is raised", {
  expect_warning(prepareSWD("Gypaetus barbatus",
                            dismo::randomPoints(env, 10000), env,
                            categorical = "biome"))
})

test_that("Output is correct", {
  swd <- prepareSWD("Gypaetus barbatus", SDMtune:::p@coords, env,
                    categorical = "biome")
  expect_s4_class(swd, "SWD")
  expect_equal(swd@species, "Gypaetus barbatus")
  expect_equal(swd@coords, SDMtune:::p@coords)
  expect_named(swd@data, names(env))
  expect_named(swd@coords, c("X", "Y"))
  expect_true(is.factor(swd@data$biome))
  expect_equal(rownames(swd@data), as.character(1:nrow(SDMtune:::p@coords)))
  expect_equal(rownames(swd@coords), as.character(1:nrow(SDMtune:::p@coords)))
  expect_equal(nrow(swd@data), nrow(SDMtune:::p@coords))
  expect_equal(nrow(swd@coords), nrow(SDMtune:::p@coords))
})
