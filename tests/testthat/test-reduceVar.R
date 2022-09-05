skip_on_cran()

val <- SDMtune:::t
m <- SDMtune:::bm_maxnet

test_that("Exceptions are raised", {
  expect_snapshot_error(reduceVar(m, th = 2, metric = "auc",
                                  test = val, use_pc = TRUE))
})

test_that("The interactive chart is not created", {
  suppressMessages(reduceVar(m, th = 2, metric = "auc", test = val, permut = 1,
                             interactive = FALSE))
  expect_false(any(grepl("SDMtune-reduceVar", list.dirs(tempdir()))))
})

test_that("Variables are reduced and interactive chart is created", {
  # Without Jackknife
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expect_message(o <- reduceVar(m, th = 2, metric = "auc", test = val,
                                permut = 1),
                 "Removed variables: bio16, bio6")
  expect_s4_class(o, "SDMmodel")
  expect_s4_class(o@model, "Maxnet")
  expect_true(min(varImp(o, 1)[, 2]) > 2)
  expect_false("bio16" %in% colnames(o@data@data))
  expect_false("bio6" %in% colnames(o@data@data))
  expect_true(any(grepl("SDMtune-reduceVar", list.dirs(tempdir()))))
  # With Jackknife
  set.seed(25, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expect_message(o <- reduceVar(m, th = 2, metric = "auc", test = val,
                                permut = 1, use_jk = TRUE, interactive = FALSE),
                 "No variable has been removed!")
  expect_s4_class(o, "SDMmodel")
  expect_s4_class(o@model, "Maxnet")
  expect_true(min(varImp(o, 1)[, 2]) < 2)
})
