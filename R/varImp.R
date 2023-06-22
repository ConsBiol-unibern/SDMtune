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
#' @param model \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @param permut integer. Number of permutations.
#' @param progress logical. If `TRUE` shows a progress bar.
#'
#' @details Note that it could return values slightly different from MaxEnt Java
#' software due to a different random permutation.
#'
#' For \linkS4class{SDMmodelCV} objects the function returns the average and the
#' standard deviation of the permutation importances of the single models.
#'
#' @return data.frame with the ordered permutation importance.
#' @export
#' @importFrom stats sd
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{# Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd",
#'                     full.names = TRUE)
#'
#' predictors <- terra::rast(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species",
#'                    p = p_coords,
#'                    a = bg_coords,
#'                    env = predictors,
#'                    categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data,
#'                          test = 0.2,
#'                          only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet",
#'                data = train,
#'                fc = "l")
#'
#' # Compute variable importance
#' vi <- varImp(model,
#'              permut = 5)
#' vi
#'
#' # Same example but using cross validation instead of training and testing
#' # datasets
#' # Create 4 random folds splitting only the presence locations
#' folds = randomFolds(data,
#'                     k = 4,
#'                     only_presence = TRUE)
#'
#' model <- train(method = "Maxnet",
#'                data = data,
#'                fc = "l",
#'                folds = folds)
#'
#' # Compute variable importance
#' vi <- varImp(model,
#'              permut = 5)
#' vi}
varImp <- function(model,
                   permut = 10,
                   progress = TRUE) {

  vars <- colnames(model@data@data)

  if (inherits(model, "SDMmodel")) {
    total <- length(vars)
  } else {
    l <- length(model@models)
    total <- length(vars) * l
  }

  if (progress)
    id <- cli::cli_progress_bar(
      name = "Variable importance",
      type = "iterator",
      format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | \\
                ETA: {cli::pb_eta} - {cli::pb_elapsed_clock}",
      total = total,
      clear = FALSE
    )

  if (inherits(model, "SDMmodel")) {
    model_auc <- auc(model)
    output <- .compute_permutation(model, model_auc, vars, permut, id, progress)
  } else {
    pis <- matrix(nrow = length(vars), ncol = l)

    for (i in 1:l) {
      model_auc <- auc(model@models[[i]])
      df <- .compute_permutation(model@models[[i]], model_auc, vars, permut, id,
                                 progress)
      index <- match(df[, 1], vars)
      pis[, i] <- df[order(index), 2]
    }

    output <- data.frame(Variable = vars,
                         Permutation_importance = rowMeans(pis),
                         sd = round(apply(pis, 1, sd), 3),
                         stringsAsFactors = FALSE)
  }

  output <- output[order(output$Permutation_importance, decreasing = TRUE), ]
  row.names(output) <- NULL

  output
}

.compute_permutation <- function(model,
                                 model_auc,
                                 vars,
                                 permut,
                                 id,
                                 progress) {

  permuted_auc <- matrix(nrow = permut, ncol = length(vars))
  set.seed(25)

  for (j in seq_along(vars)) {
    for (i in seq_len(permut)) {
      data <- sample(model@data@data[, vars[j]])
      if (is.factor(model@data@data[, vars[j]]))
        data <- as.factor(data)
      train_copy <- model@data
      train_copy@data[, vars[j]] <- data
      permuted_auc[i, j] <- auc(model, train_copy)
    }

    if (progress)
      cli::cli_progress_update(id = id, .envir = parent.frame(n = 2))
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

  output
}
