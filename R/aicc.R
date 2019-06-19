#' AICc
#'
#' Compute the Akaike Information Criterion corrected for small samples size
#' (Warren and Seifert, 2011).
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param env \code{\link[raster]{stack}} containing the environmental
#' variables.
#' @param parallel logical, if \code{TRUE} it uses parallel computation, default
#' is \code{FALSE}.
#'
#' @details Parallel computation increases the speed only for large datasets due
#' to the time necessary to create the cluster.
#'
#' @return The computed AICc
#' @export
#' @importFrom raster extract cellStats
#'
#' @author Sergio Vignali
#'
#' @references Warren D.L., Seifert S.N., (2011). Ecological niche modeling in
#' Maxent: the importance of model complexity and the performance of model
#' selection criteria. Ecological Applications, 21(2), 335–342.
#'
#' @examples
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
#' # Train a model
#' model <- train(method = "Maxnet", p = presence, a = bg, fc = "l")
#'
#' # Compute the AICc
#' aicc(model, predictors)
#'
#' \donttest{
#' # Compute the AICc using parallel computation. This reduces the time only for
#' # large datasets, in this case it takes longer than the previous example due
#' # to the time used to start and stop a cluster
#' aicc(model, predictors, parallel = TRUE)
#' }
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
