#' SDMmodel2MaxEnt
#'
#' Converts a SDMmodel object containing a Maxent model to a dismo MaxEnt
#' object.
#'
#' @param model SDMmodel object to be converted.
#'
#' @return The converted MaxEnt object.
#' @export
#'
#' @examples
#' \dontrun{SDMmodel2MaxEnt(model)}
#'
#' @author Sergio Vignali
SDMmodel2MaxEnt <- function(model) {

  if (class(model@model) != "Maxent")
    stop(paste("impossible to convert a", class(model@model),
               "in a MaxEnt object"))

  maxent_model <- new("MaxEnt")
  maxent_model@presence <- model@presence@data
  maxent_model@absence <- model@background@data
  maxent_model@lambdas <- model@model@lambdas
  maxent_model@hasabsence <- TRUE

  return(maxent_model)
}
