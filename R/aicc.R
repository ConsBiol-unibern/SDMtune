#' AICc
#'
#' Compute the Akaike Information Criterion corrected for small samples size
#' (Warren and Seifert 2011).
#'
#' @param model \link{SDMmodel} object.
#' @param env \link{stack} containing the environmental variables.
#' @param parallel logical, if TRUE it uses parallel computation, default is
#' FALSE.
#'
#' @return The computed AICc
#' @export
#' @importFrom raster extract cellStats
#'
#' @details Parallel computation increases the speed only for large datasets due
#' to the time necessary to create the cluster.
#'
#' @references Warren D.L., Seifert S.N., (2011). Ecological niche modeling in
#' Maxent: the importance of model complexity and the performance of model
#' selection criteria. Ecological Applications, 21(2), 335–342.
#'
#' @examples \dontrun{
#' aicc(model, predictors, parallel = T)}
#'
#' @author Sergio Vignali
aicc <- function(model, env, parallel = FALSE){

  # k is the number of non-zero parameter in the model
  if (class(model@model) == "Maxent") {
    k <- nrow(model@model@coeff)
    type <- "raw"
  } else {
    k <- length(model@model@model$betas)
    type <- "exponential"
  }

  if (k > nrow(model@p@data)) {
    aicc <- NA
  } else {
    raw <- predict(model, env, type = type, parallel = parallel)
    raw_sum <- raster::cellStats(raw, sum)
    values <- raster::extract(raw, model@p@coords)
    # log-likelihood of standardized presence probabilities
    loglike <- sum(log(values / raw_sum))
    # n is the number of presence observations
    n <- nrow(model@p@data)
    aic <- 2 * k - 2 * loglike
    aicc <- aic + (2 * k * (k + 1) / (n - k - 1))
    aicc <- round(aicc, 4)
  }

  return(aicc)
}
