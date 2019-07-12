#' Maxent Variable Importance
#'
#' Shows the percent contribution and permutation importance of the
#' environmental variables used to train the model.
#'
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object
#' trained using the "Maxent" method.
#'
#' @details When an \linkS4class{SDMmodelCV} object is passed to the function,
#' the output is the average of the variable importance of each model trained
#' during the cross validation.
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

  if (.get_model_class(model) != "Maxent")
    stop("'model' must be a SDMmodel object trained using the 'Maxent' method!")

  if (class(model) == "SDMmodel") {
    x <- model@model@results
    df <- .fetch_var_imp(x)
  } else {
    vars <- colnames(model@p@data)
    l <- length(model@models)
    pcs <- pis <- matrix(nrow = length(vars), ncol = l)
    for (i in 1:l) {
      x <- model@models[[i]]@model@results
      df <- .fetch_var_imp(x)
      index <- match(df[, 1], vars)
      pcs[, i] <- df[order(index), 2]
      pis[, i] <- df[order(index), 3]
    }
    df <- data.frame(x = vars, y = rowMeans(pcs), z = rowMeans(pis),
                     stringsAsFactors = FALSE)
  }
  colnames(df) <- c("Variable", "Percent_contribution",
                    "Permutation_importance")
  df <- df[order(-df$Percent_contribution), ]
  row.names(df) <- NULL
  return(df)
}

.fetch_var_imp <- function(x) {
  pc <- x[grepl("contribution", rownames(x)), ]
  pi <- x[grepl("permutation.importance", rownames(x)), ]
  variables <- gsub(".contribution", "", names(pc))
  df <- data.frame(x = variables, y = pc, z = pi, row.names = NULL,
                   stringsAsFactors = FALSE)
  return(df)
}


