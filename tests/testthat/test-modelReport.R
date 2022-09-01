skip_on_cran()
skip_on_ci()
skip_on_covr()

test_that("All files are created", {
  folder <- "trash"
  suppressMessages(create_local_model_report(folder = folder))

  expect_true(file.exists(file.path(folder, "train.csv")))
  expect_true(file.exists(file.path(folder, "test.csv")))
  expect_true(file.exists(file.path(folder, "model.Rds")))
  expect_true(file.exists(file.path(folder, "map.tif")))
  expect_true(file.exists(file.path(folder, "virtual_species.html")))
  expect_true(file.exists(file.path(folder, "plots", "ROC_curve.png")))
})
