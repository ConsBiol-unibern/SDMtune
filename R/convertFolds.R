#' Convert Folds
#'
#' Convert folds created with other packages into a format valid for
#' \pkg{SDMtune}.
#'
#' @param x list. Folds to be converted.
#' @param data \code{\linkS4class{SWD}} object used to create the folds.
#'
#' @details The function can convert folds created with the following packages:
#' ENMeval and CVblocks.
#'
#' @return list with two matrices, the first for the training and the second for
#' the testing dataset. Each column of one matrix represents a fold with
#' \code{TRUE} for the locations included in and \code{FALSE} excluded from the
#' partition.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Example using the ENMeval package
#' library(ENMeval)
#' # Block partition
#' block_folds <- get.block(occ = data@data[data@pa == 1, ],
#'                          bg.coords = data@data[data@pa == 0, ])
#' folds <- convertFolds(block_folds)
#' # Checkerboard1 partition
#' cb_folds <- get.checkerboard1(occ = data@data[data@pa == 1, ],
#'                          env = predictors,
#'                          bg.coords = data@data[data@pa == 0, ],
#'                          aggregation.factor = 4)
#' folds <- convertFolds(cb_folds)
#' }
convertFolds <- function(x, data) {
  n <- length(data@pa)

  if ("occ.grp" %in% names(x)) {
    # ENMeval fold partition
    k <- length(unique(x$occ.grp))
    train <- test <- matrix(TRUE, nrow = n, ncol = k)

    for (i in 1:k) {
      if (sum(x$bg.grp) == 0) {
        train[, i] <- c(x$occ.grp != i, rep(TRUE, length(x$bg.grp)))
        test[, i] <- c(x$occ.grp == i, rep(TRUE, length(x$bg.grp)))
      } else {
        folds <- c(x$occ.grp, x$bg.grp)
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
  }

  output <- list(train = train, test = test)

  return(output)
}
