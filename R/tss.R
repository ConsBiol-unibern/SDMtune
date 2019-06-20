#' True Skill Statistics
#'
#' Compute the max TSS of a given model.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param test \linkS4class{SWD} test locations for \linkS4class{SDMmodel}
#' objects or logical for \linkS4class{SDMmodelCV} objects, if not provided it
#' computes the training AUC, default is \code{NULL}.
#'
#' @details If the model is a \linkS4class{SDMmodelCV} object, the function
#' computes the mean of the training or testing TSS values of the different
#' replicates.
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
      if (is.null(test)) {
        data <- model@p
        data@data <- model@p@data[model@folds != i, , drop = FALSE]
      } else {
        data <- model@p
        data@data <- model@p@data[model@folds == i, , drop = FALSE]
      }
      tsss[i] <- .compute_tss(model@models[[i]], data)
    }
    tss <- mean(tsss)
  }

  return(round(tss, 4))
}

.compute_tss <- function(model, test) {

  cm <- confMatrix(model, test = test, type = "cloglog")
  tpr <- cm$tp / (cm$tp + cm$fn)
  tnr <- cm$tn / (cm$fp + cm$tn)
  tss <- tpr + tnr - 1

  return(max(tss))
}
