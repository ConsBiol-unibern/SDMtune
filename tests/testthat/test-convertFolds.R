t <- SDMtune:::t

test_that("ENMeval presence only", {
  data <- .subset_swd(t, c(rep(TRUE, 30), rep(FALSE, (length(t@pa - 30)))))
  data@pa[21:30] <- 0
  x <- list(occ.grp = cut(sample(1:20), 4, labels = FALSE), bg.grp = rep(0, 10))
  np <- 20
  n <- 30

  # ENMeval older version
  folds <- .convert_folds(x, data)

  expect_length(folds, 2)
  expect_named(folds, c("train", "test"))
  expect_equal(ncol(folds$train), 4)
  expect_equal(ncol(folds$test), 4)
  expect_equal(nrow(folds$train), nrow(folds$test))

  for (i in 1:3) {
    expect_equal(folds$train[, i][1:np], !folds$test[, i][1:np])
    expect_equal(folds$train[, i][(np + 1):n], folds$test[, i][(np + 1):n])
  }

  # ENMeval 2.x
  names(x) <- c("occs.grp", "bg.grp" )
  folds <- .convert_folds(x, data)

  expect_length(folds, 2)
  expect_named(folds, c("train", "test"))
  expect_equal(ncol(folds$train), 4)
  expect_equal(ncol(folds$test), 4)
  expect_equal(nrow(folds$train), nrow(folds$test))

  for (i in 1:3) {
    expect_equal(folds$train[, i][1:np], !folds$test[, i][1:np])
    expect_equal(folds$train[, i][(np + 1):n], folds$test[, i][(np + 1):n])
  }
})

test_that("ENMeval presence and background", {
  data <- .subset_swd(t, c(rep(TRUE, 32), rep(FALSE, (length(t@pa - 32)))))
  data@pa[21:32] <- 0
  x <- list(occ.grp = cut(sample(1:20), 4, labels = FALSE),
            bg.grp = cut(sample(1:12), 4, labels = FALSE))

  # ENMeval older version
  folds <- .convert_folds(x, data)

  expect_length(folds, 2)
  expect_named(folds, c("train", "test"))
  expect_equal(ncol(folds$train), 4)
  expect_equal(ncol(folds$test), 4)
  expect_equal(nrow(folds$train), nrow(folds$test))

  for (i in 1:3) {
    expect_equal(folds$train[, i], !folds$test[, i])
  }

  # ENMeval 2.x
  names(x) <- c("occs.grp", "bg.grp")
  folds <- .convert_folds(x, data)

  expect_length(folds, 2)
  expect_named(folds, c("train", "test"))
  expect_equal(ncol(folds$train), 4)
  expect_equal(ncol(folds$test), 4)
  expect_equal(nrow(folds$train), nrow(folds$test))

  for (i in 1:3) {
    expect_equal(folds$train[, i], !folds$test[, i])
  }
})

test_that("blockCV", {
  data <- .subset_swd(t, c(rep(TRUE, 30), rep(FALSE, (length(t@pa - 30)))))
  data@pa[21:30] <- 0
  f <- vector("list", length = 2)
  for (i in 1:2) {
    f[[i]] <- vector("list", length = 2)
    s <- sample(30, 20)
    f[[i]][1] <- list(s)
    f[[i]][2] <- list(setdiff(1:30, s))
  }

  # blockCV 2.x
  x <- list(folds = f, k = 2)
  class(x) <- "EnvironmentalBlock"
  folds <- .convert_folds(x, data)

  expect_length(folds, 2)
  expect_named(folds, c("train", "test"))
  expect_equal(ncol(folds$train), 2)
  expect_equal(ncol(folds$test), 2)
  expect_equal(nrow(folds$train), nrow(folds$test))

  for (i in 1:2) {
    expect_equal(folds$train[, i], !folds$test[, i])
    expect_equal(sort(unlist(f[[i]][1])), which(folds$train[, i]))
    expect_equal(sort(unlist(f[[i]][2])), which(folds$test[, i]))
  }

  # blockCV 3.x
  x <- list(folds_list = f, k = 2)
  class(x) <- "cv_spatial"
  folds <- .convert_folds(x, data)

  expect_length(folds, 2)
  expect_named(folds, c("train", "test"))
  expect_equal(ncol(folds$train), 2)
  expect_equal(ncol(folds$test), 2)
  expect_equal(nrow(folds$train), nrow(folds$test))

  for (i in 1:2) {
    expect_equal(folds$train[, i], !folds$test[, i])
    expect_equal(sort(unlist(f[[i]][1])), which(folds$train[, i]))
    expect_equal(sort(unlist(f[[i]][2])), which(folds$test[, i]))
  }
})

test_that("randomFolds", {
  f <- randomFolds(t, 2)
  expect_equal(f, .convert_folds(f, t))
})

test_that("error is raised", {
  expect_snapshot_error(.convert_folds(t, t))
})
