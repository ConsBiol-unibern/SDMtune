skip_on_cran()

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd", full.names = TRUE)
env <- raster::stack(files)
# TODO Change when new data are available
p <- .subset_swd(SDMtune:::t, fold = as.logical(SDMtune:::t@pa))@coords
set.seed(25)
a <- suppressWarnings(dismo::randomPoints(env, 10000))

# TODO remove test for old version
test_that("Warning is raised", {
  expect_error(prepareSWD(species = "Gypaetus barbatus", coords = a,
                          env = env, categorical = "biome"), "deprecated")
})

test_that("Output is correct", {
  swd <- expect_message(prepareSWD(species = "Gypaetus barbatus", p = p, a = a,
                                   env = env, categorical = "biome"), "Info:")
  expect_s4_class(swd, "SWD")
  expect_equal(swd@species, "Gypaetus barbatus")
  expect_named(swd@data, names(env))
  expect_named(swd@coords, c("X", "Y"))
  expect_true(is.factor(swd@data$biome))
  expect_equal(rownames(swd@data), as.character(1:length(swd@pa)))
  expect_equal(rownames(swd@coords), as.character(1:length(swd@pa)))
  expect_equal(nrow(swd@data), length(swd@pa))
  expect_equal(nrow(swd@coords), length(swd@pa))
})

test_that("The function works with only presences or only absences data", {
  expect_s4_class(swd <- prepareSWD(species = "Gypaetus barbatus", p = p,
                                    env = env, categorical = "biome"), "SWD")
  expect_true(unique(swd@pa) == 1)
  expect_s4_class(swd <- prepareSWD(species = "Gypaetus barbatus", a = a,
                                    env = env, categorical = "biome"), "SWD")
  expect_true(unique(swd@pa) == 0)
})
