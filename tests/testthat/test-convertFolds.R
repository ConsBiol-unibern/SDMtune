t <- SDMtune:::t

test_that("ENMeval presence only", {
  data <- .subset_swd(t, c(rep(TRUE, 30), rep(FALSE, (length(t@pa - 30)))))
  x <- list(occ.grp = cut(sample(1:20), 4, labels = FALSE), bg.grp = rep(0, 10))
  folds <- convertFolds(x, data)
  np <- 20
  n <- 30
  expect_length(folds, 2)
  expect_named(folds, c("train", "test"))
  expect_equal(ncol(folds$train), ncol(folds$test), 4)
  expect_equal(nrow(folds$train), nrow(folds$test))
  for (i in 1:3) {
    expect_equal(folds$train[, i][1:np], !folds$test[, i][1:np])
    expect_equal(folds$train[, i][(np + 1):n], folds$test[, i][(np + 1):n])
  }
})

test_that("ENMeval presence and background", {
  data <- .subset_swd(t, c(rep(TRUE, 32), rep(FALSE, (length(t@pa - 32)))))
  x <- list(occ.grp = cut(sample(1:20), 4, labels = FALSE),
            bg.grp = cut(sample(1:12), 4, labels = FALSE))
  folds <- convertFolds(x, data)
  expect_length(folds, 2)
  expect_named(folds, c("train", "test"))
  expect_equal(ncol(folds$train), ncol(folds$test), 4)
  expect_equal(nrow(folds$train), nrow(folds$test))
  for (i in 1:3) {
    expect_equal(folds$train[, i], !folds$test[, i])
  }
})
