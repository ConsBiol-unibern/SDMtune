#' SDMmodel2MaxEnt
#'
#' Converts an \code{\linkS4class{SDMmodel}} object containing a
#' \code{\linkS4class{Maxent}} model into a dismo \code{\linkS4class{MaxEnt}}
#' object.
#'
#' @param model \code{\linkS4class{SDMmodel}} object to be converted.
#'
#' @return The converted dismo \code{\linkS4class{MaxEnt}} object.
#' @export
#'
#' @examples
#' \donttest{
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
#' # Train a Maxent model
#' model <- train(method = "Maxent", p = presence, a = bg, fc = "l")
#'
#' dismo_model <- SDMmodel2MaxEnt(model)
#' dismo::evaluate(p = presence@data, a = bg@data, model = dismo_model)
#' }
#'
#' @author Sergio Vignali
SDMmodel2MaxEnt <- function(model) {

  if (class(model@model) != "Maxent")
    stop("'model' must be a SDMmodel object trained using the 'Maxent' method!")

  maxent_model <- new("MaxEnt")
  maxent_model@presence <- .get_presence(model@data)
  maxent_model@absence <- .get_absence(model@data)
  maxent_model@lambdas <- model@model@lambdas
  maxent_model@hasabsence <- TRUE

  return(maxent_model)
}
