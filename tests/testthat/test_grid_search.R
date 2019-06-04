context("Grid Search")

h <- list(fc = c("l", "q", "p"))
o <- gridSearch(SDMtune:::bm_maxnet, h, "auc", SDMtune:::p)
m_cv <- train("Maxnet", SDMtune:::p, SDMtune:::bg_model, fc = "l", rep = 2)

test_that("gridSearch produces the expected output", {
  expect_s4_class(o, "SDMtune")
  expect_s4_class(o@models[[1]], "SDMmodel")
  expect_s3_class(o@results, "data.frame")
  expect_named(o@results, c("a", "fc", "reg", "train_AUC", "test_AUC",
                            "diff_AUC"))
  expect_length(o@results$a, 3)
  expect_length(o@models, 3)
  expect_equal(o@results$a, c(5000, 5000, 5000))
  expect_equal(o@results$fc, c("l", "q", "p"))
  expect_equal(o@results$reg, c(1, 1, 1))
})

test_that("Show method for SDMtune class produces the correct output", {
  expect_output(print(o), "Object of class:  SDMtune", fixed = TRUE)
  expect_output(print(o), "a: 5000", fixed = TRUE)
  expect_output(print(o), "fc: l, p, q", fixed = TRUE)
  expect_output(print(o), "reg: 1", fixed = TRUE)
})

h <- list(fc = c("l", "q"), a = c(2000, 4000))
o <- gridSearch(m_cv, h, "auc", bg4test = SDMtune:::bg_model,
                test = SDMtune:::p, save_models = FALSE)

test_that("gridSearch produces the expected output with cross validation", {
  expect_s4_class(o, "SDMtune")
  expect_s3_class(o@results, "data.frame")
  expect_named(o@results, c("a", "fc", "reg", "train_AUC", "test_AUC",
                            "diff_AUC"))
  expect_length(o@results$a, 4)
  expect_length(o@models, 1)
  expect_equal(o@results$a, c(2000, 4000, 2000, 4000))
  expect_equal(o@results$fc, c("l", "l", "q", "q"))
  expect_equal(o@results$reg, c(1, 1, 1, 1))
})
