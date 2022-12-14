skip_on_cran()

data <- SDMtune:::t
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)
m <- SDMtune:::bm_maxnet
folds <- randomFolds(data,
                     k = 2,
                     only_presence = TRUE)

m_cv <- train("Maxnet",
              data = data,
              fc = "l",
              folds = folds)

h <- list(fc = c("l", "q"))
o <- gridSearch(m,
                hypers = h,
                metric = "auc",
                test = data,
                interactive = FALSE,
                progress = FALSE)

test_that("gridSearch produces the expected output", {
  expect_s4_class(o, "SDMtune")
  expect_s4_class(o@models[[1]], "SDMmodel")
  expect_s3_class(o@results, "data.frame")
  expect_named(o@results, c("fc", "reg", "train_AUC", "test_AUC", "diff_AUC"))
  expect_length(o@models, 2)
  expect_equal(o@results$fc, c("l", "q"))
  expect_equal(o@results$reg, c(1, 1))
  expect_false(any(grepl("SDMtune-gridSearch", list.dirs(tempdir()))))
})

test_that("Show method for SDMtune class produces the correct output", {
  expect_output(print(o), "Object of class:  SDMtune", fixed = TRUE)
  expect_output(print(o), "fc: l, q", fixed = TRUE)
  expect_output(print(o), "reg: 1", fixed = TRUE)
})

test_that("gridSearch produces the expected output with AICc", {
  o <- gridSearch(m,
                  hypers = h,
                  metric = "aicc",
                  test = data,
                  env = predictors,
                  save_models = FALSE,
                  interactive = FALSE,
                  progress = FALSE)

  expect_s4_class(o, "SDMtune")
  expect_s4_class(o@models[[1]], "SDMmodel")
  expect_s3_class(o@results, "data.frame")
  expect_named(o@results, c("fc", "reg", "AICc", "delta_AICc"))
  expect_length(o@models, 1)
  expect_equal(o@results$fc, c("l", "q"))
  expect_equal(o@results$reg, c(1, 1))
  expect_false(any(grepl("SDMtune-gridSearch", list.dirs(tempdir()))))
})

test_that("gridSearch produces the expected output with cross validation", {
  h <- list(fc = c("l", "q"),
            reg = c(1, 2))

  o <- gridSearch(m_cv,
                  hypers = h,
                  metric = "auc",
                  test = data,
                  save_models = FALSE,
                  progress = FALSE)

  expect_s3_class(o@results, "data.frame")
  expect_named(o@results, c("fc", "reg", "train_AUC", "test_AUC", "diff_AUC"))
  expect_length(o@models, 1)
  expect_equal(o@results$fc, c("l", "q", "l", "q"))
  expect_equal(o@results$reg, c(1, 1, 2, 2))
  expect_true(any(grepl("SDMtune-gridSearch", list.dirs(tempdir()))))
})

test_that("The function raises errors", {
  expect_snapshot_error(gridSearch(m,
                                   hypers = h,
                                   metric = "aicc",
                                   test = data,
                                   save_models = FALSE,
                                   env = "spam",
                                   interactive = FALSE,
                                   progress = FALSE))
})

# TODO: Remove with version 2.0.0
test_that("The function warns if a raster object is used", {
  env <- raster::stack(files)
  expect_snapshot_warning(gridSearch(m,
                                     hypers = h,
                                     metric = "aicc",
                                     test = data,
                                     save_models = FALSE,
                                     env = env,
                                     interactive = FALSE,
                                     progress = FALSE))
})
