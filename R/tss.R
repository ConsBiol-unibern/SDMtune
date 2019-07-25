#' True Skill Statistics
#'
#' Compute the max TSS of a given model.
#'
#' @param model \code{\linkS4class{SDMmodel}} or \code{\linkS4class{SDMmodelCV}}
#' object.
#' @param test \code{\linkS4class{SWD}} test locations for
#' \code{\linkS4class{SDMmodel}} objects or logical for
#' \code{\linkS4class{SDMmodelCV}} objects, if not provided it computes the
#' training AUC, default is \code{NULL}.
#'
#' @details If the model is a \code{\linkS4class{SDMmodelCV}} object, the
#' function computes the mean of the training or testing TSS values of the
#' different replicates.
#'
#' @return The value of the TSS of the given model.
#' @export
#'
#' @author Sergio Vignali
#'
#' @references
#' Allouche O., Tsoar A., Kadmon R., (2006). Assessing the accuracy of species
#' distribution models: prevalence, kappa and the true skill statistic (TSS).
#' Journal of Applied Ecology, 43(6), 1223–1232.
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
#' # Compute the training TSS
#' tss(model)
#'
#' # Compute the testing TSS
#' tss(model, test)
#'
#' \donttest{
#' # Same example but using cross validation instead of training and testing
#' # datasets
#' model <- train(method = "Maxnet", p = presence, a = bg, fc = "l", rep = 4,
#'                seed = 25)
#'
#' # Compute the training TSS
#' TSS(model)
#'
#' # Compute the testing TSS
#' TSS(model, test = TRUE)
#' }
tss <- function(model, test = NULL) {

  if (class(model) == "SDMmodel") {
    tss <- .compute_tss(model, test)
  } else {
    tsss <- vector("numeric", length = length(model@models))

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
      tsss[i] <- .compute_tss(model@models[[i]], test_swd)
    }
    tss <- mean(tsss)
  }

  return(tss)
}

.compute_tss <- function(model, test) {

  if (class(model@model) == "Maxent") {
    type <- "raw"
  } else {
    type <- "link"
  }

  cm <- confMatrix(model, test = test, type = type)
  tpr <- cm$tp / (cm$tp + cm$fn)
  tnr <- cm$tn / (cm$fp + cm$tn)
  tss <- tpr + tnr - 1

  return(max(tss))
}
