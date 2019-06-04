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

test_that(".create_sdmtune_result", {
  # Produce the correct result with auc
  expect_equal(.create_sdmtune_result(model, metric = "auc", train_metric = 0.9,
                                      val_metric = 0.8),
               list(a = 5000, fc = "lqph", reg = 1, train_AUC = 0.9,
                    test_AUC = 0.8, diff_AUC = 0.1))
  # Produce the correct result with tss
  expect_equal(.create_sdmtune_result(model, metric = "tss", train_metric = 0.9,
                                      val_metric = 0.8),
               list(a = 5000, fc = "lqph", reg = 1, train_TSS = 0.9,
                    test_TSS = 0.8, diff_TSS = 0.1))
  # Produce the correct result with aicc
  expect_equal(.create_sdmtune_result(model, metric = "aicc",
                                      train_metric = 0.9, val_metric = NA),
               list(a = 5000, fc = "lqph", reg = 1, AICc = 0.9,
                    delta_AICc = NA))
  # Produce the correct output type
  expect_type(.create_sdmtune_result(model, metric = "aicc",
                                     train_metric = 0.9, val_metric = NA),
              "list")
  # Produce the correct result with SDMmodelCV
  expect_equal(.create_sdmtune_result(model_cv, metric = "aicc",
                                      train_metric = 0.9, val_metric = NA),
               list(a = 5000, fc = "lqph", reg = 1, AICc = 0.9,
                    delta_AICc = NA))
})

test_that(".create_sdm_output", {
  # Produce the correct output type
  expect_s4_class(.create_sdmtune_output(list(model, model), metric = "auc",
                                         train_metric = data.frame(x = c(1, 2),
                                                                   y = c(.8, .9)),
                                         val_metric = data.frame(x = c(1, 2),
                                                                 y = c(.6, .8))),
                                         "SDMtune")
  # Produce the correct result with aicc
  expect_length(.create_sdmtune_output(list(model, model), metric = "aicc",
                                      train_metric = data.frame(x = c(1, 2),
                                                                y = c(.8, .9)),
                                      val_metric = NA)@models, 2)
  # Produce the correct result with auc
  expect_equal(.create_sdmtune_output(list(model, model), metric = "auc",
                                      train_metric = data.frame(x = c(1, 2),
                                                                y = c(.8, .9)),
                                      val_metric = data.frame(x = c(1, 2),
                                                              y = c(.6, .8))
                                      )@results$diff_AUC, c(.2, .1))
  # Produce the correct result with tss
  expect_equal(.create_sdmtune_output(list(model, model), metric = "tss",
                                      train_metric = data.frame(x = c(1, 2),
                                                                y = c(.8, .9)),
                                      val_metric = data.frame(x = c(1, 2),
                                                              y = c(.6, .8))
  )@results$diff_TSS, c(.2, .1))
  # Produce the correct result with aicc
  expect_equal(.create_sdmtune_output(list(model, model), metric = "aicc",
                                      train_metric = data.frame(x = c(1, 2),
                                                                y = c(.8, .9)),
                                      val_metric = NA)@results$delta_AICc,
               c(0, .1))
  # Produce the correct result with SDMmodelCV
  expect_length(.create_sdmtune_output(list(model_cv, model_cv),
                                       metric = "aicc",
                                       train_metric = data.frame(x = c(1, 2),
                                                                 y = c(.8, .9)),
                                       val_metric = NA)@models, 2)
})

test_that(".get_train_args", {
  # Give the correct output using maxnet
  expect_named(.get_train_args(model),
               c("p", "a", "rep", "method", "fc", "reg"))
  # Give the correct output using maxent
  expect_named(.get_train_args(model_mx),
               c("p", "a", "rep", "method", "fc", "reg", "iter", "extra_args"))
  # Give corret rep argument
  expect_equal(.get_train_args(model)$rep, 1)
  expect_equal(.get_train_args(model_cv)$rep, 4)
  # Give the correct output type
  expect_type(.get_train_args(model), "list")
})

test_that("get_tunable_args", {
  expect_equal(get_tunable_args(model_mx), c("a", "fc", "reg", "iter"))
  expect_equal(get_tunable_args(model), c("a", "fc", "reg"))
})

test_that(".create_model_from_settings", {
  m <- .create_model_from_settings(model, list(fc = "l", reg = 2))
  expect_s4_class(m, "SDMmodel")
  expect_s4_class(m@model, "Maxnet")
  expect_equal(m@model@fc, "l")
  expect_equal(m@model@reg, 2)
  expect_warning(
    .create_model_from_settings(model, list(fc = "l", reg = 2, a = 6000)),
    "Ignored number of 'a' in settings!")
  m <- .create_model_from_settings(model, list(fc = "l", reg = 2, a = 6000),
                                   bg4test = SDMtune:::bg,
                                   bg_folds = sample(1:nrow(SDMtune:::bg@data)))
  expect_equal(nrow(m@a@data), 6000)
  expect_equal(rownames(m@a@data), as.character(1:6000))
  expect_equal(rownames(m@a@coords), as.character(1:6000))
})

test_that("The function .get_hypers_grid generates the correct grid", {
  expect_type(.get_hypers_grid(model, h)$a, "integer")
  expect_type(.get_hypers_grid(model, h), "list")
})
