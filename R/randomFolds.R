#' Create Random Folds
#'
#' Create random folds for cross validation.
#'
#' @param data \linkS4class{SWD} object that will be used to train the model.
#' @param k integer. Number of fold used to create the partition.
#' @param only_presence logical, if `TRUE` the random folds are created only for
#' the presence locations and all the background locations are included in each
#' fold, used manly for presence-only methods.
#' @param seed integer. The value used to set the seed for the fold partition.
#'
#' @details When `only_presence = FALSE`, the proportion of presence and absence
#' is preserved.
#'
#' @return list with two matrices, the first for the training and the second for
#' the testing dataset. Each column of one matrix represents a fold with
#' `TRUE` for the locations included in and `FALSE` excluded from the partition.
#'
#' @export
#'
#' @author Sergio Vignali
#' @examples
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
#' # Create 4 random folds splitting presence and absence locations
#' folds <- randomFolds(data, k = 4)
#'
#' # Create 4 random folds splitting only the presence locations
#' folds <- randomFolds(data, k = 4, only_presence = TRUE)
randomFolds <- function(data,
                        k,
                        only_presence = FALSE,
                        seed = NULL) {

  if (!inherits(data, "SWD"))
    cli::cli_abort(c(
      "!" = "{.var data} must be an {.cls SWD} object",
      "x" = "You have supplied a {.cls {class(data)}} instead."
    ))

  if (!is.null(seed))
    set.seed(seed)

  train <- test <- matrix(TRUE, nrow = length(data@pa), ncol = k)
  p_folds <- cut(sample(seq_len(nrow(.get_presence(data)))), k, labels = FALSE)
  a_folds <- cut(sample(seq_len(nrow(.get_absence(data)))), k, labels = FALSE)

  for (i in 1:k) {
    if (only_presence) {
      train[, i] <- c(p_folds != i, rep(TRUE, nrow(.get_absence(data))))
      test[, i] <- c(p_folds == i, rep(TRUE, nrow(.get_absence(data))))
    } else {
      folds <- c(p_folds, a_folds)
      train[, i] <- folds != i
      test[, i] <- folds == i
    }
  }

  output <- list(train = train, test = test)

  return(output)
}
