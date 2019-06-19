#' Train
#'
#' Train a model using the given method.
#'
#' @param method character. Possible values are "Maxent" or "Maxnet".
#' @param p \linkS4class{SWD} object with the presence locations.
#' @param a \linkS4class{SWD} object with the absence or background locations.
#' @param rep numeric. Number of replicates, used for cross validation.
#' Default is 1, meaning no cross validation is performed.
#' @param verbose logical if \code{TRUE} shows a progress bar if replicates are
#' greater then 1, default is \code{TRUE}.
#' @param folds numeric. Vector containing the indexes for the k-fold partition
#' of the training data, if not provided the function randomly creates the
#' folds, default is \code{NULL}.
#' @param seed integer. The value used to set the seed for the fold partition,
#' used if **folds** is not provided, default is \code{NULL}.
#' @param ... Arguments passed to the relative method, see details.
#'
#' @details
#' * For the Maxent method, possible arguments are:
#'     + reg: numeric. The value of the regularization multiplier, default is 1.
#'     + fc: vector. The value of the feature classes, possible values are
#'       combinations of "l", "q", "p", "h" and "t", default is "lqph".
#'     + iter: numeric. Number of iterations used by the MaxEnt algorithm,
#'       default is 500.
#'     + extra_args: vector. Extra arguments used to run MaxEnt, default is
#'       "removeduplicates=false" and "addsamplestobackground=false". In case
#'       this is not your expected behavior you can assign extra_args = "" or
#'       you can change or add any other additional arguments extending the
#'       default settings (e.g. \code{extra_args = c("removeduplicates=true,
#'       addsamplestobackground=true)"**})
#' * For the Maxnet method, possible arguments are:
#'     + reg: numeric. The value of the regularization intensity, default is 1.
#'     + fc: vector. The value of the feature classes, possible values are
#'       combinations of "l", "q", "p", "h" and "t", default is "lqph". For more
#'       details see \code{\link[maxnet]{maxnet}}.
#'
#' @return An \linkS4class{SDMmodel} or \linkS4class{SDMmodelCV} object.
#' @export
#' @importFrom dismo kfold
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
#' # Train a Maxent model
#' model <- train(method = "Maxent", p = train, a = bg, fc = "l", reg = 1.5,
#'                iter = 700)
#'
#' # Train a Maxnet model
#' model <- train(method = "Maxnet", p = train, a = bg, fc = "lq", reg = 1.5)
#'
#' # Train a Maxnet model with cross validation
#' model <- train(method = "Maxnet", p = train, a = bg, fc = "l", reg = 0.8,
#'                rep = 4)
#' }
train <- function(method = c("Maxent", "Maxnet"), p, a, rep = 1, verbose = TRUE,
                  folds = NULL, seed = NULL, ...) {
  method <- match.arg(method)
  f <- paste0("train", method)

  if (rep == 1) {
    model <- do.call(f, args = list(p = p, a = a, ...))
  } else {
    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "Cross Validation [:bar] :percent in :elapsedfull",
        total = rep, clear = FALSE, width = 60, show_after = 0)
      pb$tick(0)
    }
    models <- vector("list", rep)
    if (is.null(folds)) {
      if (!is.null(seed))
        set.seed(seed)
      folds <- cut(sample(1:nrow(p@data)), rep, labels = FALSE)
    }
    for (i in 1:rep) {
      train <- p
      train@data <- p@data[folds != i, , drop = FALSE]
      models[[i]] <- do.call(f, args = list(p = train, a = a, ...))
      if (verbose)
        pb$tick(1)
    }
    model <- SDMmodelCV(models = models, p = p, a = a,
                        folds = folds)
  }

  return(model)
}
