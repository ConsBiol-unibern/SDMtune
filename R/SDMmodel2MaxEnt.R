#' SDMmodel2MaxEnt
#'
#' Converts a \link{SDMmodel} object containing a \link{Maxent} model to a dismo
#' \link{MaxEnt} object.
#'
#' @param model \link{SDMmodel} object to be converted.
#'
#' @return The converted \link{MaxEnt} object.
#' @export
#'
#' @examples
#' \dontrun{SDMmodel2MaxEnt(model)}
#'
#' @author Sergio Vignali
SDMmodel2MaxEnt <- function(model) {

  if (class(model@model) != "Maxent")
    stop("'model' must be a SDMmodel object trained using the 'Maxent' method!")

  maxent_model <- new("MaxEnt")
  maxent_model@presence <- model@p@data
  maxent_model@absence <- model@a@data
  maxent_model@lambdas <- model@model@lambdas
  maxent_model@hasabsence <- TRUE

  return(maxent_model)
}
