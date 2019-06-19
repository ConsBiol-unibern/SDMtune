#' Maxent Variable Importance
#'
#' Shows the percent contribution and permutation importance of the
#' environmental variables used to train the model.
#'
#' @param model \linkS4class{SDMmodel} object trained using the "Maxent" method.
#'
#' @return A data frame with the variable importance.
#' @export
#'
#' @author Sergio Vignali
#'
#' @seealso \code{\link{maxentTh}}
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
#' maxentVarImp(model)
#' }
maxentVarImp <- function(model) {

  if (class(model) == "SDMmodelCV")
    stop("Function not available for SDMmodelCV!")

  if (class(model@model) != "Maxent")
    stop("'model' must be a SDMmodel object trained using the 'Maxent' method!")

  input <- model@model@results
  pc <- input[grepl("contribution", rownames(input)), ]
  pi <- input[grepl("permutation.importance", rownames(input)), ]
  variables <- gsub(".contribution", "", names(pc))
  df <- data.frame(Variable = variables, Percent_contribution = round(pc, 1),
                   Permutation_importance = round(pi, 1),
                   row.names = NULL, stringsAsFactors = FALSE)

  df <- df[order(-df$Percent_contribution), ]
  row.names(df) <- NULL
  return(df)
}
