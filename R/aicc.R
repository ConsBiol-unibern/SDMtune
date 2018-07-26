#' AICc
#'
#' Compute Akaike Information Criterion corrected for small samples size
#' \insertCite{Warren2011}{SDMSelection}
#'
#' @param model Maxent object.
#' @param env \link{stack} or \link{brick} containing the environmental variables.
#' @param parallel logical, if TRUE it uses parallel computation, deafult is FALSE.
#'
#' @return The computed AICc
#' @export
#' @importFrom raster extract cellStats
#' @importFrom Rdpack reprompt
#'
#' @details You need package \link{snow} to use parallel computation and \link{rgdal}
#' to save the prediction in a raster file. Parallel computation increases the speed
#' only for big datasets due to the time necessary to create the cluster.
#'
#' @references \insertRef{Warren2011}{SDMSelection}
#'
#' @examples \dontrun{
#' aicc(model, predictors, parallel = T)}
#'
#' @author Sergio Vignali
aicc <- function(model, env, parallel = FALSE){

  k <- nrow(model@coeff)
  raw <- predict(model, env, type = "raw", parallel = parallel)
  raw_sum <- raster::cellStats(raw, sum)
  values <- raster::extract(raw, model@presence@coords)
  ll <- sum(log(values / raw_sum))
  aic <- 2 * k - 2 * ll
  aicc <- aic + (2 * k * (k + 1) / (nrow(model@presence@data) - k - 1))

  return(round(aicc, 4))
}
