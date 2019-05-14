context("Utils")

model <- SDMtune:::bm_maxnet
model_mx <- SDMtune:::bm_maxent
model_cv <- SDMtune:::bm_maxnet_cv
h <- list(fc = c("l", "lq", "lqp"), reg = seq(.2, 2., .2))

test_that(".get_model_class", {
  expect_equivalent(.get_model_class(model), "Maxnet")
  expect_equivalent(.get_model_class(model_cv), "Maxnet")
})

test_that(".get_model_reg", {
  expect_equal(.get_model_reg(model), 1)
  expect_equal(.get_model_reg(model_cv), 1)
})

test_that(".get_model_fc", {
  expect_equal(.get_model_fc(model), "lqph")
  expect_equal(.get_model_fc(model_cv), "lqph")
})

test_that(".get_model_hyperparams", {
  expect_equal(.get_model_hyperparams(model), "Reg: 1 FC: lqph #Bg: 5000")
  expect_equal(.get_model_hyperparams(model_cv), "Reg: 1 FC: lqph #Bg: 5000")
})

test_that(".get_footer", {
  expect_equal(.get_footer(model), "a: 5000\nfc: lqph\nreg: 1")
  expect_equal(.get_footer(model_cv), "a: 5000\nfc: lqph\nreg: 1")
  expect_equal(.get_footer(model_mx), "a: 5000\nfc: lqph\nreg: 1\niter: 500")
})

test_that(".get_total_model", {
  expect_equal(.get_total_models(20, 12, 5), 80)
})

test_that(".get_metric_label", {
  expect_equal(.get_metric_label("auc"), "AUC")
  expect_equal(.get_metric_label("tss"), "TSS")
  expect_equal(.get_metric_label("aicc"), "AICc")
})

test_that(".get_sdmtune_colnames", {
  expect_equal(.get_sdmtune_colnames("auc"),
               c("train_AUC", "test_AUC", "diff_AUC"))
  expect_equal(.get_sdmtune_colnames("tss"),
               c("train_TSS", "test_TSS", "diff_TSS"))
  expect_equal(.get_sdmtune_colnames("aicc"), c("AICc", "delta_AICc"))
})

test_that("The function .get_hypers_grid generates the correct grid", {
  expect_type(.get_hypers_grid(model, h)$a, "integer")
})
