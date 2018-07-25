#' AICc
#'
#' @param model Maxent object.
#' @param env
#' @param parallel logical, if TRUE it uses parallel computation, deafult is FALSE.
#'
#' @return The computed AICc
#' @export
#' @importFrom raster extract cellStats
#'
#' @examples\dontrun{
#' aicc(model, predictors, parallel = T)}
#'
#' @author Sergio Vignali
aicc <- function(model, env, parallel = FALSE){

  k <- nrow(model@coeff)
  raw <- predict(model, env, type = "raw", parallel = parallel)
  raw_sum <- raster::cellStats(raw, sum)
  raw_norm <- raw / raw_sum
  vals <- raster::extract(raw_norm, model@presence@coords)
  ll <- sum(log(vals))
  aic <- 2 * k - 2 * ll
  aicc <- aic + (2 * k * (k + 1) / (nrow(model@presence@data) - k - 1))

  return(aicc)
}
