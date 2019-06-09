context("Reduce Variables")

skip_on_cran()

val <- SDMtune:::p
m <- SDMtune:::bm_maxnet

test_that("Exceptions are raised", {
  expect_error(reduceVar(m, th = 2, metric = "auc", test = val, use_pc = TRUE),
               "Percent contribution cannot be used with model of method Maxnet")
})

test_that("Variable are reduced", {
  # Without Jackknife
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expect_message(o <- reduceVar(m, th = 2, metric = "auc", test = val,
                                permut = 1),
                 "Removed variables: bio12")
  expect_s4_class(o, "SDMmodel")
  expect_s4_class(o@model, "Maxnet")
  expect_true(min(varImp(o, 1)[, 2]) > 2)
  expect_false("bio12" %in% colnames(o@p@data))
  expect_false("bio12" %in% colnames(o@a@data))
  # With Jackknife
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expect_message(o <- reduceVar(m, th = 2, metric = "auc", test = val, permut = 1,
                 use_jk = TRUE), "No variable is removed!")
  expect_s4_class(o, "SDMmodel")
  expect_s4_class(o@model, "Maxnet")
  expect_true(min(varImp(o, 1)[, 2]) < 2)
})
