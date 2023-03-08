skip_on_cran()

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

env <- terra::rast(files)
p <- virtualSp$presence
# Add coordinates outside extent to get info message
p <- rbind(c(10, 10), p)
a <- virtualSp$background

test_that("Output is correct", {
  swd <- suppressWarnings(prepareSWD(species = "Gypaetus barbatus",
                                     p = p,
                                     a = a,
                                     env = env,
                                     categorical = "biome",
                                     verbose = FALSE))
  expect_s4_class(swd, "SWD")
  expect_equal(swd@species, "Gypaetus barbatus")
  expect_named(swd@data, names(env))
  expect_named(swd@coords, c("X", "Y"))
  expect_true(is.factor(swd@data$biome))
  expect_equal(rownames(swd@data), as.character(seq_along(swd@pa)))
  expect_equal(rownames(swd@coords), as.character(seq_along(swd@pa)))
  expect_equal(nrow(swd@data), length(swd@pa))
  expect_equal(nrow(swd@coords), length(swd@pa))
})

test_that("The function works with only presences or only absences data", {
  expect_s4_class(swd <- suppressWarnings(
    prepareSWD(species = "Gypaetus barbatus",
               p = p,
               env = env,
               categorical = "biome",
               verbose = FALSE)),
    "SWD")
  expect_true(unique(swd@pa) == 1)
  expect_s4_class(swd <- suppressWarnings(
    prepareSWD(species = "Gypaetus barbatus",
               a = a,
               env = env,
               categorical = "biome",
               verbose = FALSE)),
    "SWD")
  expect_true(unique(swd@pa) == 0)
})

test_that("The function warns if some locations are discarded", {
  # One location
  a <- rbind(a, c(100, 100))
  expect_snapshot_warning(prepareSWD(species = "Bgs",
                                     a = a,
                                     env = env,
                                     verbose = FALSE))

  # More than one location
  a <- rbind(a, c(100, 100))
  expect_snapshot_warning(prepareSWD(species = "Bgs",
                                     a = a,
                                     env = env,
                                     verbose = FALSE))
})

test_that("The function raises errors", {
  expect_snapshot_error(prepareSWD(species = "Bgs",
                                   a = a,
                                   env = "spam"))
})

# TODO: Remove with version 2.0.0
test_that("The function raises an error if a raster object is used", {
  class(env) <- "Raster"
  expect_snapshot_error(prepareSWD(species = "Bgs",
                                   a = a,
                                   env = env,
                                   verbose = FALSE))
})
