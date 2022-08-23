#' AICc
#'
#' Compute the Akaike Information Criterion corrected for small samples size
#' (Warren and Seifert, 2011).
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param env \link[raster]{stack} containing the environmental
#' variables.
#'
#' @details The function is available only for **Maxent** and **Maxnet**
#' methods.
#'
#' @return The computed AICc
#' @export
#'
#' @author Sergio Vignali
#'
#' @references Warren D.L., Seifert S.N., (2011). Ecological niche modeling in
#' Maxent: the importance of model complexity and the performance of model
#' selection criteria. Ecological Applications, 21(2), 335–342.
#'
#' @seealso \link{auc} and \link{tss}.
#'
#' @examples
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
#' # Train a model
#' model <- train(method = "Maxnet", data = data, fc = "l")
#'
#' # Compute the AICc
#' aicc(model, predictors)
aicc <- function(model, env) {

  if (!inherits(model@model, c("Maxent", "Maxnet")))
    stop("AICc available only for \"Maxent\" and \"Maxnet\" methods.")

  # k is the number of non-zero parameter in the model
  if (inherits(model@model, "Maxent")) {
    k <- nrow(model@model@coeff)
    type <- "raw"
  } else {
    k <- length(model@model@model$betas)
    type <- "exponential"
  }

  if (k > sum(model@data@pa == 1)) {
    aicc <- NA
  } else {
    raw <- predict(model, env, type = type)
    raw_sum <- raster::cellStats(raw, sum)
    values <- raster::extract(raw, model@data@coords[model@data@pa == 1, ])
    # log-likelihood of standardized presence probabilities
    loglike <- sum(log(values / raw_sum))
    # n is the number of presence observations
    n <- sum(model@data@pa == 1)
    aic <- 2 * k - 2 * loglike
    aicc <- aic + (2 * k * (k + 1) / (n - k - 1))
  }

  return(aicc)
}
