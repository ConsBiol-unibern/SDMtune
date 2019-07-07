#' AUC
#'
#' Compute the AUC using the Man-Whitney U Test formula.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param test \linkS4class{SWD} test locations for \linkS4class{SDMmodel}
#' objects or logical.
#' for \linkS4class{SDMmodelCV} objects, if not provided it computes the train
#' AUC, default is \code{NULL}.
#' @param a \linkS4class{SWD} absence or background locations used to compute
#' the AUC by the permutation importance function, default is \code{NULL}.
#'
#' @details If the model is a \linkS4class{SDMmodelCV} object, the function
#' computes the mean of the training or testing AUC values of the different
#' replicates.
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
#' # Prepare presence locations
#' p_coords <- condor[, 1:2]
#'
#' # Prepare background locations
#' bg_coords <- dismo::randomPoints(predictors, 5000)
#'
#' # Create SWD object
#' presence <- prepareSWD(species = "Vultur gryphus", coords = p_coords,
#'                        env = predictors, categorical = "biome")
#' bg <- prepareSWD(species = "Vultur gryphus", coords = bg_coords,
#'                  env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(presence, test = 0.2)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", p = train, a = bg, fc = "l")
#'
#' # Compute the training AUC
#' auc(model)
#'
#' # Compute the testing AUC
#' auc(model, test)
#'
#' \donttest{
#' # Same example but using cross validation instead of training and testing
#' # datasets
#' model <- train(method = "Maxnet", p = presence, a = bg, fc = "l", rep = 4,
#'                seed = 25)
#'
#' # Compute the training AUC
#' auc(model)
#'
#' # Compute the testing AUC
#' auc(model, test = TRUE)
#' }
auc <- function(model, test = NULL, a = NULL) {

  if (class(model) == "SDMmodel") {
    auc <- .compute_auc(model, test, a)
  } else {
    aucs <- vector("numeric", length = length(model@models))
    data <- model@p
    for (i in 1:length(model@models)) {
      if (is.null(test)) {
        data@data <- model@p@data[model@folds != i, , drop = FALSE]
      } else {
        data@data <- model@p@data[model@folds == i, , drop = FALSE]
      }
      aucs[i] <- .compute_auc(model@models[[i]], data, a)
    }
    auc <- mean(aucs)
  }

  return(auc)
}

.compute_auc <- function(model, test, a) {

  if (class(model@model) == "Maxent") {
    type <- "raw"
  } else {
    type <- "link"
  }

  if (is.null(test)) {
    p <- model@p@data
  } else {
    p <- test@data[colnames(model@p@data)]
  }

  # a is used for permutation importance
  if (is.null(a)) {
    a <- model@a@data
  } else {
    a <- a@data[colnames(model@p@data)]
  }

  pred <- predict(model, rbind(p, a), type = type)
  n_p <- nrow(p)
  n_a <- nrow(a)

  # AUC using the Mann-Whitney U Test
  Rp <- sum(rank(pred)[1:n_p])  # Sum of rank of positive cases
  Up <- Rp - (n_p * (n_p + 1) / 2)  # U test for positive cases
  auc <- Up / (n_p * n_a)

  return(auc)
}
