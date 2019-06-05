context("Plot ROC")

p <- plotROC(SDMtune:::bm_maxent, val = SDMtune:::p, test = SDMtune:::p)

test_that("The plot contains the correct data", {
  expect_equal(unique(p$data$set), c("train", "val", "test"))
  expect_equal(min(p$data$fpr[p$data$set == "train"]), 0)
  expect_equal(max(p$data$fpr[p$data$set == "train"]), 1)
  expect_equal(min(p$data$tpr[p$data$set == "train"]), 0)
  expect_equal(max(p$data$tpr[p$data$set == "train"]), 1)
  expect_equal(min(p$data$fpr[p$data$set == "val"]), 0)
  expect_equal(max(p$data$fpr[p$data$set == "val"]), 1)
  expect_equal(min(p$data$tpr[p$data$set == "val"]), 0)
  expect_equal(max(p$data$tpr[p$data$set == "val"]), 1)
  expect_equal(min(p$data$fpr[p$data$set == "test"]), 0)
  expect_equal(max(p$data$fpr[p$data$set == "test"]), 1)
  expect_equal(min(p$data$tpr[p$data$set == "test"]), 0)
  expect_equal(max(p$data$tpr[p$data$set == "test"]), 1)

})

test_that("The plot has the correct labels", {
  expect_equal(p$labels$x, "False Positive Rate")
  expect_equal(p$labels$y, "True Positive Rate")
  expect_true(grepl("Train", p$plot_env$labels[1]))
  expect_true(grepl("Val", p$plot_env$labels[2]))
  expect_true(grepl("Test", p$plot_env$labels[3]))
})
