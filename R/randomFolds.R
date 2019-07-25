#' Create Random Folds
#'
#' Create random folds for cross validation.
#'
#' @param data \code{\linkS4class{SWD}} object that will be used to train the
#' model.
#' @param k integer. Number of fold used to create the partition.
#' @param only_presence logical, if \code{TRUE} the random folds are created
#' only for the presence locations, used manly for presence-only methods,
#' default is \code{FALSE}.
#' @param seed integer. The value used to set the seed for the fold partition,
#' default is \code{NULL}.
#'
#' @return list with two matrices, the first for the training and the second for
#' the testing dataset. Each column of one matrix represents a fold with
#' \code{TRUE} for the locations included in and \code{FALSE} excluded from the
#' partition.
#' @export
#'
#' @author Sergio Vignali
#' @examples
randomFolds <- function(data, k, only_presence = FALSE, seed = NULL) {

  if (class(data) != "SWD")
    stop("\"data\" argument is not of class SWD.")

  if (!is.null(seed))
    set.seed(seed)

  train <- test <- matrix(TRUE, nrow = length(data@pa), ncol = k)
  p_folds <- cut(sample(1:nrow(.get_presence(data))), k, labels = FALSE)
  a_folds <- cut(sample(1:nrow(.get_absence(data))), k, labels = FALSE)

  for (i in 1:k) {
    if (only_presence) {
      train[, i] <- c(p_folds != k, rep(TRUE, nrow(.get_absence(data))))
      test[, i] <- c(p_folds == k, rep(TRUE, nrow(.get_absence(data))))
    } else {
      folds <- c(p_folds, a_folds)
      train[, i] <- folds != k
      test[, i] <- folds == k
    }
  }

  output <- list(train = train, test = test)

  return(output)
}
