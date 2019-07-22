files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
env <- raster::stack(files)
p <- SDMtune:::p@coords
a <- suppressWarnings(dismo::randomPoints(env, 10000))

# TODO remove it
# Test for old version
test_that("Warning is raised", {
  expect_warning(prepareSWD(species = "Gypaetus barbatus", coords = a,
                            env = env, categorical = "biome"), "deprecated")
})

test_that("Output is correct", {
  swd <- expect_warning(prepareSWD(species = "Gypaetus barbatus", coords = p,
                                   env = env, categorical = "biome"))
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

# Test for new version
test_that("Output is correct", {
  swd <- expect_message(prepareSWD(species = "Gypaetus barbatus", p = p, a = a,
                                   env = env, categorical = "biome"),
                        "Warning:")
  expect_s4_class(swd, "SWD")
  expect_equal(swd@species, "Gypaetus barbatus")
  expect_named(swd@data, names(env))
  expect_named(swd@coords, c("X", "Y"))
  expect_true(is.factor(swd@data$biome))
  expect_equal(rownames(swd@data), as.character(1:length(swd@pa)))
  expect_equal(rownames(swd@coords), as.character(1:length(swd@pa)))
  expect_equal(nrow(swd@data),length(swd@pa))
  expect_equal(nrow(swd@coords), length(swd@pa))
})
