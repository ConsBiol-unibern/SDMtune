#' True Skill Statistics
#'
#' Compute the max TSS of a given model.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param test \linkS4class{SWD} object when `model` is an
#' \linkS4class{SDMmodel} object; logical or \linkS4class{SWD} object when
#' `model` is an \linkS4class{SDMmodelCV} object. If not provided it computes
#' the training TSS, see details. Default is `NULL`.
#'
#' @details For \code{\linkS4class{SDMmodelCV}} objects, the function computes
#' the mean of the training TSS values of the k-folds. If \code{test = TRUE} it
#' computes the mean of the testing TSS values for the k-folds. If test is an
#' \code{\linkS4class{SWD}} object, it computes the mean TSS values for the
#' provided testing dataset.
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
#' @seealso \link{aicc} and \link{auc}.
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
#' # Compute the training TSS
#' tss(model)
#'
#' # Compute the testing TSS
#' tss(model, test)
#'
#' \donttest{
#' # Same example but using cross validation instead of training and testing
#' # datasets
#' # Create 4 random folds splitting only the presence locations
#' folds = randomFolds(train, k = 4, only_presence = TRUE)
#' model <- train(method = "Maxnet", data = train, fc = "l", folds = folds)
#'
#' # Compute the training TSS
#' tss(model)
#'
#' # Compute the testing TSS
#' tss(model, test = TRUE)
#'
#' # Compute the TSS for the held apart testing dataset
#' tss(model, test = test)
#' }
tss <- function(model, test = NULL) {

  if (inherits(model, "SDMmodel")) {
    tss <- .compute_tss(model, test)
  } else {
    tsss <- vector("numeric", length = length(model@models))

    for (i in seq_along(model@models)) {
      if (!is.null(test)) {
        if (isTRUE(test)) {
          test_swd <- .subset_swd(model@data, model@folds$test[, i])
        } else {
          test_swd <- test
        }
      } else {
        test_swd <- NULL
      }
      tsss[i] <- .compute_tss(model@models[[i]], test_swd)
    }
    tss <- mean(tsss)
  }

  return(tss)
}

.compute_tss <- function(model, test) {

  if (inherits(model@model, "Maxent")) {
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
