#' Combine Cross Validation models
#'
#' This function combines cross-validation models by retraining a new model with
#' all presence and absence/background locations and the same hyperparameters.
#'
#' @details This is an utility function to retrain a model with all data after,
#' for example, the hyperparameters tuning (\link{gridSearch},
#' \link{randomSearch} or \link{optimizeModel}) to avoid manual setting of the
#' hyperparameters in the \link{train} function.
#'
#' @param model \linkS4class{SDMmodelCV} object.
#'
#' @return An \linkS4class{SDMmodel} object.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{# Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species",
#'                    p = p_coords,
#'                    a = bg_coords,
#'                    env = predictors,
#'                    categorical = "biome")
#'
#' # Create 4 random folds splitting only the presence data
#' folds <- randomFolds(data,
#'                      k = 4,
#'                      only_presence = TRUE)
#'
#' model <- train(method = "Maxnet",
#'                data = data,
#'                folds = folds)
#'
#' # Define the hyperparameters to test
#' h <- list(reg = 1:2,
#'           fc = c("lqp", "lqph"))
#'
#' # Run the function using the AUC as metric
#' output <- gridSearch(model,
#'                      hypers = h,
#'                      metric = "auc")
#' output@results
#' output@models
#'
#' # Order results by highest test AUC
#' output@results[order(-output@results$test_AUC), ]
#'
#' # Combine cross validation models for output with highest test AUC
#' idx <- which.max(output@results$test_AUC)
#' combined_model <- combineCV(output@models[[idx]])
#' combined_model}
combineCV <- function(model) {
  args <- .get_train_args(model)
  args$folds <- NULL

  do.call("train", args)
}
