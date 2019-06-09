context("Variable Selection")

skip_on_cran()

t <- SDMtune:::p
bg <- SDMtune:::bg
m <- SDMtune:::bm_maxnet

test_that("Exceptions are thrown", {
  expect_error(varSel(m, metric = "auc", bg4cor = bg, test = t, use_pc = TRUE),
               "Percent contribution cannot be used with model of method Maxnet")
})

test_that("Correlated Variable are removed", {
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expect_message(o <- varSel(m, "auc", bg, test = t, cor_th = .9, permut = 1),
                 "Removed variables: bio1, bio12")
  expect_s4_class(o, "SDMmodel")
  expect_s4_class(o@model, "Maxnet")
  expect_false("bio1" %in% colnames(o@p@data))
  expect_false("bio1" %in% colnames(o@a@data))
  expect_false("bio12" %in% colnames(o@p@data))
  expect_false("bio12" %in% colnames(o@a@data))
})
