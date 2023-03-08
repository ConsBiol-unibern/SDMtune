skip_on_cran()

val <- SDMtune:::t
m <- SDMtune:::bm_maxnet

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)

test_that("The interactive chart is not created", {
  reduceVar(m,
            th = 2,
            metric = "auc",
            test = val,
            permut = 1,
            interactive = FALSE,
            verbose = FALSE)
  expect_false(any(grepl("SDMtune-reduceVar", list.dirs(tempdir()))))
})

test_that("Variables are reduced and interactive chart is created", {
  # Without Jackknife
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expect_message(o <- reduceVar(m,
                                th = 2,
                                metric = "auc",
                                test = val,
                                permut = 1),
                 "The variables bio16 and bio6 have been removed")
  expect_s4_class(o, "SDMmodel")
  expect_s4_class(o@model, "Maxnet")
  expect_true(min(varImp(o, 1)[, 2]) > 2)
  expect_false("bio16" %in% colnames(o@data@data))
  expect_false("bio6" %in% colnames(o@data@data))
  expect_true(any(grepl("SDMtune-reduceVar", list.dirs(tempdir()))))

  # With Jackknife
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expect_message(o <- reduceVar(m,
                                th = 2,
                                metric = "auc",
                                test = val,
                                permut = 1,
                                use_jk = TRUE,
                                interactive = FALSE),
                 "No variables  have been removed")
  expect_s4_class(o, "SDMmodel")
  expect_s4_class(o@model, "Maxnet")
  expect_true(min(varImp(o, 1)[, 2]) < 2)
})

test_that("Exceptions are raised", {
  expect_snapshot_error(reduceVar(m,
                                  th = 2,
                                  metric = "auc",
                                  test = val,
                                  use_pc = TRUE))

  expect_snapshot_error(reduceVar(m,
                                  th = 2,
                                  metric = "aicc",
                                  env = "spam",
                                  test = val,
                                  permut = 1))
})

# TODO: Remove with version 2.0.0
test_that("The function raises an error if a raster object is used", {
  class(predictors) <- "Raster"
  expect_snapshot_error(reduceVar(m,
                                  th = 2,
                                  metric = "aicc",
                                  env = predictors,
                                  test = val,
                                  permut = 1,
                                  verbose = FALSE))
})
