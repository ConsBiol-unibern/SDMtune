data <- SDMtune:::t
model <- SDMtune:::bm_maxnet
model_mx <- SDMtune:::bm_maxent
model_cv <- SDMtune:::bm_maxnet_cv
model_ann <- train("ANN", data = data, size = 10)
model_rf <- train("RF", data = data)
model_brt <- train("BRT", data = data)

test_that("The correct arguments are returned", {
  expect_equal(getTunableArgs(model_mx), c("fc", "reg", "iter"))
  expect_equal(getTunableArgs(model), c("fc", "reg"))
  expect_equal(getTunableArgs(model_ann), c("size", "decay", "rang", "maxit"))
  expect_equal(getTunableArgs(model_rf), c("mtry", "ntree", "nodesize"))
  expect_equal(getTunableArgs(model_brt),
               c("distribution", "n.trees", "interaction.depth", "shrinkage",
                 "bag.fraction"))
})
