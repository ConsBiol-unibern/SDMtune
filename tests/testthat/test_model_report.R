context("Model Report")

skip_on_cran()

folder <- "trash"
m <- SDMtune:::bm_maxnet

modelReport(m, type = "cloglog", folder = folder, test = SDMtune:::p,
            permut = 1)
test_that("The files are created", {
  expect_true(file.exists(file.path(folder, "background.csv")))
  expect_true(file.exists(file.path(folder, "presence.csv")))
  expect_true(file.exists(file.path(folder, "test.csv")))
  expect_true(file.exists(file.path(folder, "model.Rds")))
  expect_true(file.exists(file.path(folder, "vultur_gryphus.html")))
  expect_true(file.exists(file.path(folder, "plots", "ROC_curve.png")))
})

teardown(unlink(file.path(getwd(), folder), recursive = TRUE))
