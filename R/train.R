#' Train
#'
#' Train a model using one of the following methods: BRT, Maxent, Maxnet or
#' Random Forest.
#'
#' @param method character. Method used to train the model, possible values are
#' "Maxent", "Maxnet" or "RF".
#' @param data \code{\linkS4class{SWD}} object with presence and
#' absence/background locations.
#' @param folds list with two matrices, the first for the training and the
#' second for the testing dataset. Each column of one matrix represents a fold
#' with \code{TRUE} for the locations included in and \code{FALSE} excluded from
#' the partition.
#' @param verbose logical, if \code{TRUE} shows a progress bar during cross
#' validation, default is \code{TRUE}.
#' @param p Deprecated.
#' @param a Deprecated.
#' @param rep Deprecated.
#' @param seed Deprecated.
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
#' * For the Maxnet method, possible arguments are (for more details see
#' \code{\link[maxnet]{maxnet}}):
#'     + reg: numeric. The value of the regularization intensity, default is 1.
#'     + fc: vector. The value of the feature classes, possible values are
#'       combinations of "l", "q", "p", "h" and "t", default is "lqph".
#' * For the ANN method possible arguments are (for more details see
#' \code{\link[nnet]{nnet}}):
#'     + size: integer. Number of the units in the hidden layer.
#'     + decay numeric. Weight decay, default is 0.
#'     + rang numeric. Initial random weights, default is 0.7.
#'     + maxit integer. Maximum number of iterations, default is 100.
#' * For the RF method the model is trained as classification. Possible
#' arguments are (for more details see
#' \code{\link[randomForest]{randomForest}}):
#'     + mtry: integer. Number of variable randomly sampled at each split,
#'     default is \code{floor(sqrt(number of variables))}.
#'     + ntree: integer. Number of tree to grow, default is 500.
#'     + nodesize: integer. Minimum size of terminal nodes.
#' * For the BRT method possible arguments are (for more details see
#' \code{\link[gbm]{gbm}}):
#'     + distribution: character. Name of the distribution to use, default is
#'     "bernoulli".
#'     + ntree: integer. Maximum number of tree to grow, default is 100.
#'     + interaction.depth: integer. Maximum depth of each tree, default is 1.
#'     + lr: numeric. The shrinkage parameter, default is 0.1.
#'     + bag.fraction: numeric. Random fraction of data used in the tree
#'     expansion, default is 0.5.
#'
#' @return An \code{\linkS4class{SDMmodel}} or \code{\linkS4class{SDMmodelCV}}
#' object.
#' @export
#' @importFrom progress progress_bar
#'
#' @seealso \code{\link{randomFolds}} \code{\link{convertFolds}}
#'
#' @author Sergio Vignali
#'
#' @references Hijmans, Robert J., Steven Phillips, John Leathwick, and Jane
#' Elith. 2017. dismo: Species Distribution Modeling.
#' \url{https://cran.r-project.org/package=dismo}.
#'
#' Steven Phillips (2017). maxnet: Fitting 'Maxent' Species Distribution Models
#' with 'glmnet'. \url{https://CRAN.R-project.org/package=maxnet}.
#'
#' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S.
#' Fourth Edition. Springer, New York. ISBN 0-387-95457-0
#'
#' A. Liaw and M. Wiener (2002). Classification and Regression by randomForest.
#' R News 2(3), 18--22.
#'
#' Brandon Greenwell, Bradley Boehmke, Jay Cunningham and GBM Developers (2019).
#' gbm: Generalized Boosted Regression Models.
#' \url{https://CRAN.R-project.org/package=gbm}
#'
#' @examples
#' \donttest{
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
#' # Train a Maxent model
#' model <- train(method = "Maxent", data = data, fc = "l", reg = 1.5,
#'                iter = 700)
#'
#' # Train a Maxnet model
#' model <- train(method = "Maxnet", data = data, fc = "lq", reg = 1.5)
#'
#' # Train a Maxnet model with cross validation
#' # Create 4 random folds splitting only the presence data
#' folds <- randomFolds(data, k = 4, only_presence = TRUE)
#' model <- train(method = "Maxnet", data = data, fc = "l", reg = 0.8,
#'                folds = folds)
#'
#' # Train presence absence models
#' # Prepare presence and absence locations
#' p_coords <- virtualSp$presence
#' a_coords <- virtualSp$absence#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = a_coords,
#'                    env = predictors[[1:5]])
#'
#' # Train an Artificial Neural Network model
#' model <- train("ANN", data = data, size = 10)
#'
#' # Train a Random Forest model
#' model <- train("RF", data = data, ntree = 300)
#'
#' # Train a Boosted Regression Tree model
#' model <- train("BRT", data = data, ntree = 300, lr = 0.001)
#' }
train <- function(method, data, folds = NULL, verbose = TRUE, p = NULL,
                  a = NULL, rep = NULL, seed = NULL, ...) {

  method <- match.arg(method, c("Maxent", "Maxnet", "ANN", "RF", "BRT"))
  f <- paste0("train", method)

  # TODO Remove in next release
  if (!is.null(p) & !is.null(a)) {
    stop("Argument \"p\" and \"a\" are deprecated, use \"data\" instead.")
  }
  if (!is.null(rep))
    warning("Argument \"rep\" is deprecated and will be removed in the nex ",
            "release. The number of partition is taken from the \"fold\" ",
            "argument.")
  if (!is.null(seed))
    warning("Argument seed is deprecated and will be removed in the next ",
            "release.", call. = FALSE)

  if (is.null(folds)) {
    model <- do.call(f, args = list(data = data, ...))
  } else {
    k <- ncol(folds[[1]])
    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "Cross Validation [:bar] :percent in :elapsedfull",
        total = k, clear = FALSE, width = 60, show_after = 0)
      pb$tick(0)
    }

    models <- vector("list", length = k)

    for (i in 1:k) {
      train <- .subset_swd(data, folds$train[, i])
      models[[i]] <- do.call(f, args = list(data = train, ...))
      if (verbose)
        pb$tick(1)
    }

    model <- SDMmodelCV(models = models, data = data, folds = folds)
  }

  return(model)
}
