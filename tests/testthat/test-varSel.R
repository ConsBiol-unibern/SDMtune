skip_on_cran()

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)
set.seed(25)

bg <- terra::spatSample(predictors,
                        size = 9000,
                        method = "random",
                        na.rm = TRUE,
                        xy = TRUE,
                        values = FALSE)

bg <- prepareSWD(species = "Bgs",
                 a = bg, env = predictors,
                 categorical = "biome",
                 verbose = FALSE)

t <- SDMtune:::t
m <- SDMtune:::bm_maxnet

test_that("The interactive chart is not created", {
  varSel(m,
         metric = "auc",
         bg4cor = bg,
         test = t,
         cor_th = .9,
         permut = 1,
         interactive = FALSE,
         verbose = FALSE)

  expect_false(any(grepl("SDMtune-varSel", list.dirs(tempdir()))))
})

test_that("Correlated Variable are removed and interactive chart is created", {
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expect_message(o <- varSel(m,
                             metric = "auc",
                             bg4cor = bg,
                             test = t,
                             cor_th = .9,
                             permut = 1),
                 "The variables bio16 and bio6 have been removed")
  expect_s4_class(o, "SDMmodel")
  expect_s4_class(o@model, "Maxnet")
  expect_false("bio16" %in% colnames(o@data@data))
  expect_false("bio6" %in% colnames(o@data@data))
  expect_true(any(grepl("SDMtune-varSel", list.dirs(tempdir()))))
})

test_that("Exceptions are thrown", {
  expect_snapshot_error(varSel(m,
                               metric = "auc",
                               bg4cor = bg,
                               test = t,
                               use_pc = TRUE))

  expect_snapshot_error(varSel(m,
                               metric = "aicc",
                               bg4cor = bg,
                               test = t,
                               env = "spam"))
})

# TODO: Remove with version 2.0.0
test_that("The function raises an error if a raster object is used", {
  class(predictors) <- "Raster"
  expect_snapshot_error(varSel(m,
                               metric = "aicc",
                               bg4cor = bg,
                               test = t,
                               env = predictors,
                               verbose = FALSE))
})
