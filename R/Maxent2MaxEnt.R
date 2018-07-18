#' Maxent2MaxEnt
#'
#' Converts a Maxent object to a dismo MaxEnt object.
#'
#' @param model The Maxent object to be converted.
#'
#' @return The converted MaxEnt object.
#' @export
#'
#' @examples
#' \dontrun{MaxentModel2MaxEnt(model)}
#'
#' @author Sergio Vignali
Maxent2MaxEnt <- function(model) {
  maxent_model <- new('MaxEnt')
  maxent_model@presence <- model@presence@data
  maxent_model@absence <- model@background@data
  maxent_model@lambdas <- model@lambdas
  maxent_model@hasabsence <- TRUE

  return(maxent_model)
}
