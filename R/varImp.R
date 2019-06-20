#' Variable Importance
#'
#' The function randomly permutes one variable at time (using training and
#' absence/background datasets) and computes the decrease in training AUC. The
#' result is normalized to percentages. Same implementation of MaxEnt java
#' software but with the additional possibility of running several permutations
#' to obtain a better estimate of the permutation importance. In case of more
#' than one permutation (default is 10) the average of the decrease in training
#' AUC is computed.
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param permut integer. Number of permutations, default is 10.
#'
#' @details Note that it could return values slightly different from MaxEnt java
#' software due to a different random permutation.
#'
#' @return data.frame with the ordered permutation importance.
#' @export
#' @importFrom stats sd
#' @importFrom progress progress_bar
#'
#' @author Sergio Vignali
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
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(presence, test = 0.2)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", p = presence, a = bg, fc = "l")
#'
#' # Compute variable importance
#' vi <- varImp(model, permut = 5)
#' vi
#' }
varImp <- function(model, permut = 10) {

  set.seed(25)
  vars <- colnames(model@p@data)
  model_auc <- auc(model)
  permuted_auc <- matrix(nrow = permut, ncol = length(vars))

  n_pres <- nrow(model@p@data)

  pb <- progress::progress_bar$new(
    format = "Variable importance [:bar] :percent in :elapsedfull",
    total = length(vars),
    clear = FALSE, width = 60, show_after = 0)
  pb$tick(0)

  for (j in 1:length(vars)) {
    for (i in 1:permut) {
      data <- sample(c(model@p@data[, vars[j]],
                       model@a@data[, vars[j]]))
      if (is.factor(model@p@data[, vars[j]]))
        data <- as.factor(data)
      p_copy <- model@p
      p_copy@data[, vars[j]] <- data[1:n_pres]
      a_copy <- model@a
      a_copy@data[, vars[j]] <- data[(n_pres + 1):length(data)]
      permuted_auc[i, j] <- auc(model, p_copy, a = a_copy)
    }
    pb$tick(1)
  }

  if (permut > 1) {
    sd_auc <- apply(permuted_auc, 2, sd)
    permuted_auc <- apply(permuted_auc, 2, mean)
  }

  perm_imp <- pmax(0, (model_auc - permuted_auc))
  perm_imp <- 100 * perm_imp / sum(perm_imp)
  perm_imp <- round(perm_imp, 1)

  output <- data.frame(Variable = vars, Permutation_importance = perm_imp,
                       stringsAsFactors = FALSE)
  if (permut > 1)
    output$sd <- round(sd_auc, 3)
  output <- output[order(output$Permutation_importance, decreasing = TRUE), ]
  row.names(output) <- NULL

  return(output)
}
