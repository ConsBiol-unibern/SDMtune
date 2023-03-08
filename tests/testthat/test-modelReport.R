skip_on_cran()

files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)
folder <- "trash"

test_that("All files are created", {

  withr::defer(unlink(file.path(getwd(), folder), recursive = TRUE))

  modelReport(SDMtune:::bm_maxnet,
              type = "cloglog",
              folder = folder,
              test = SDMtune:::t,
              permut = 1,
              env = predictors,
              jk = TRUE,
              response_curves = TRUE,
              verbose = FALSE)

  expect_true(file.exists(file.path(folder, "train.csv")))
  expect_true(file.exists(file.path(folder, "test.csv")))
  expect_true(file.exists(file.path(folder, "model.Rds")))
  expect_true(file.exists(file.path(folder, "map.tif")))
  expect_true(file.exists(file.path(folder, "virtual_species.html")))
  expect_true(file.exists(file.path(folder, "plots", "ROC_curve.png")))
  expect_true(file.exists(file.path(folder, "plots", "map.png")))
  expect_true(file.exists(file.path(folder, "plots", "train_jk.png")))
  expect_true(file.exists(file.path(folder, "plots", "test_jk.png")))

  for (var in names(predictors)) {
    expect_true(file.exists(
      file.path(folder, "plots", paste0(var, "_marginal.png"))
    ))
    expect_true(file.exists(
      file.path(folder, "plots", paste0(var, "_univariate.png"))
    ))
  }
})

test_that("Settings are correct", {
  data <- SDMtune:::t
  data@data <- data@data[, 1:4]
  m_ann <- train("ANN", data = data, size = 10)
  m_brt <- train("BRT", data = data, n.trees = 200, shrinkage = 0.2)
  m_rf <- train("RF", data = data, mtry = 2, ntree = 200)
  params = list(model = SDMtune:::bm_maxent,
                type = "cloglog",
                test = SDMtune:::t,
                folder = folder,
                plot_folder = file.path(folder, "plots"),
                env = predictors,
                jk = FALSE,
                response_curves = FALSE,
                only_presence = FALSE,
                clamp = TRUE,
                permut = 1,
                factors = NULL,
                verbose = FALSE)

  # Maxent with training and testing datasets and prediction
  expect_snapshot_output(.write_report_model_settings(params))

  # Maxnet without testing datasets and prediction
  params$model <- SDMtune:::bm_maxnet
  params$test <- NULL
  params$env <- NULL
  expect_snapshot_output(.write_report_model_settings(params))

  # ANN
  params$model <- m_ann
  expect_snapshot_output(.write_report_model_settings(params))

  # BRT
  params$model <- m_brt
  expect_snapshot_output(.write_report_model_settings(params))

  # RF
  params$model <- m_rf
  expect_snapshot_output(.write_report_model_settings(params))
})
