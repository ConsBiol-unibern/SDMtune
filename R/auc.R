#' AUC
#'
#' Compute the AUC using the Man-Whitney U Test formula.
#'
#' @param model \code{\linkS4class{SDMmodel}} or
#' \code{\linkS4class{SDMmodelCV}} objects.
#' @param test \code{\linkS4class{SWD}} test object for
#' \code{\linkS4class{SDMmodel}} objects or logical for
#' \code{\linkS4class{SDMmodelCV}} objects, if not provided it computes the
#' train AUC, default is \code{NULL}.
#' @param a Deprecated.
#'
#' @details If the model is a \code{\linkS4class{SDMmodelCV}} object, the
#' function computes the mean of the training or testing AUC values of the
#' different replicates.
#'
#' @return The value of the AUC.
#' @export
#'
#' @author Sergio Vignali
#'
#' @references Mason, S. J. and Graham, N. E. (2002), Areas beneath the relative
#' operating characteristics (ROC) and relative operating levels (ROL) curves:
#' Statistical significance and interpretation. Q.J.R. Meteorol. Soc., 128:
#' 2145-2166.
#'
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
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", data = train, fc = "l")
#'
#' # Compute the training AUC
#' auc(model)
#'
#' # Compute the testing AUC
#' auc(model, test = test)
#'
#' \donttest{
#' # Same example but using cross validation instead of training and testing
#' # datasets
#' # Create the folds
#' folds <- randomFolds(data, k = 4, only_presence = TRUE)
#' model <- train(method = "Maxnet", data = data, fc = "l", folds = folds)
#'
#' # Compute the training AUC
#' auc(model)
#'
#' # Compute the testing AUC
#' auc(model, test = TRUE)
#' }
auc <- function(model, test = NULL, a = NULL) {

  # TODO remove it in next release
  if (!is.null(a))
    warning("Argument \"a\" is deprecated and not used anymore, it will be ",
            "removed in the next release")

  if (class(model) == "SDMmodel") {
    auc <- .compute_auc(model, test)
  } else {
    aucs <- vector("numeric", length = length(model@models))
    for (i in 1:length(model@models)) {

      if (!is.null(test)) {
        if (isTRUE(test)) {
          test_swd <- .subset_swd(model@data, model@folds$test[, i])
        } else {
          stop("\"test\" argument invalid for \"SDMmodelCV\" objects! Use ",
               "TRUE or FALSE.")
        }
      } else {
        test_swd = NULL
      }

      aucs[i] <- .compute_auc(model@models[[i]], test_swd)
    }
    auc <- mean(aucs)
  }

  return(auc)
}

.compute_auc <- function(model, test) {

  if (class(model@model) == "Maxent") {
    type <- "raw"
  } else {
    type <- "link"
  }

  if (is.null(test)) {
    data <- model@data
  } else {
    # TODO check if can be removed: test@data[colnames(model@p@data)]
    data <- test
  }

  pred <- predict(model, data = data, type = type)
  n_p <- sum(data@pa == 1)
  n_a <- sum(data@pa == 0)

  # AUC using the Mann-Whitney U Test
  Rp <- sum(rank(pred)[1:n_p])  # Sum of rank of positive cases
  Up <- Rp - (n_p * (n_p + 1) / 2)  # U test for positive cases
  auc <- Up / (n_p * n_a)

  return(auc)
}
