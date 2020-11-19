#' SDMmodel2MaxEnt
#'
#' Converts an \linkS4class{SDMmodel} object containing a \linkS4class{Maxent}
#' model into a dismo \linkS4class{MaxEnt} object.
#'
#' @param model \linkS4class{SDMmodel} object to be converted.
#'
#' @return The converted dismo \linkS4class{MaxEnt} object.
#' @export
#' @importFrom dismo maxent
#'
#' @examples
#' \donttest{
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
#' # Train a Maxent model
#' # The next line checks if Maxent is correctly configured but you don't need
#' # to run it in your script
#' if (dismo::maxent(silent = TRUE)) {
#' model <- train(method = "Maxent", data = data, fc = "l")
#'
#' dismo_model <- SDMmodel2MaxEnt(model)
#' dismo_model
#' }
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
