#' Get Tunable Arguments
#'
#' Returns the name of all function arguments that can be tuned for a given
#' model.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#'
#' @return character vector.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Acquire environmental variables
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
#' # Train a Maxnet model and get tunable hyperparameters
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "l")
#'
#' getTunableArgs(model)
getTunableArgs <- function(model) {

  if (inherits(model, "SDMmodelCV")) {
    method <- class(model@models[[1]]@model)
  } else {
    method <- class(model@model)
  }

  if (method == "Maxent") {
    args <- c("fc", "reg", "iter")
  } else if (method == "Maxnet") {
    args <- c("fc", "reg")
  } else if (method == "ANN") {
    args <- c("size", "decay", "rang", "maxit")
  } else if (method == "RF") (
    args <- c("mtry", "ntree", "nodesize")
  ) else {
    args <- c("distribution", "n.trees", "interaction.depth", "shrinkage",
              "bag.fraction")
  }

  return(args)
}
