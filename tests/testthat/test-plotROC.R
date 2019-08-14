p <- plotROC(SDMtune:::bm_maxent, test = SDMtune:::t)

test_that("The plot contains the correct data", {
  expect_equal(unique(p$data$set), c("Train", "Test"))
  expect_equal(min(p$data$fpr[p$data$set == "Train"]), 0)
  expect_equal(max(p$data$fpr[p$data$set == "Train"]), 1)
  expect_equal(min(p$data$tpr[p$data$set == "Train"]), 0)
  expect_equal(max(p$data$tpr[p$data$set == "Train"]), 1)
  expect_equal(min(p$data$fpr[p$data$set == "Test"]), 0)
  expect_equal(max(p$data$fpr[p$data$set == "Test"]), 1)
  expect_equal(min(p$data$tpr[p$data$set == "Test"]), 0)
  expect_equal(max(p$data$tpr[p$data$set == "Test"]), 1)

})

test_that("The plot has the correct labels", {
  expect_equal(p$labels$x, "False Positive Rate")
  expect_equal(p$labels$y, "True Positive Rate")
})

test_that("error is raised", {
  expect_error(p <- plotROC(SDMtune:::bm_maxent, val = SDMtune:::t),
  "\"val\" argument is deprecated and will be removed in the next ")
})
