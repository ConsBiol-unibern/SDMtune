.convert_folds <- function(x,
                           data) {
  n <- length(data@pa)

  if ("train" %in% names(x) & "test" %in% names(x)) {
    return(x)
  } else if (any(grepl("occ", names(x)))) {
    # ENMeval fold partition
    occ <- names(x)[1]
    k <- length(unique(x[[occ]]))
    train <- test <- matrix(TRUE, nrow = n, ncol = k)

    for (i in 1:k) {
      if (sum(x$bg.grp) == 0) {
        train[, i] <- c(x[[occ]] != i, rep(TRUE, length(x$bg.grp)))
        test[, i] <- c(x[[occ]] == i, rep(TRUE, length(x$bg.grp)))
      } else {
        folds <- c(x[[occ]], x$bg.grp)
        train[, i] <- folds != i
        test[, i] <- folds == i
      }
    }
  } else if (class(x) %in% c("SpatialBlock", "BufferedBlock",
                             "EnvironmentalBlock")) {
    # blockCV fold partition
    k <- x$k
    train <- test <- matrix(FALSE, nrow = n, ncol = k)
    for (i in 1:k) {
      train[unlist(x$folds[[i]][1]), i] <- TRUE
      test[unlist(x$folds[[i]][2]), i] <- TRUE
    }
  } else {
    cli::cli_abort("Folds object format not allowed.")
  }

  list(train = train,
       test = test)

}
