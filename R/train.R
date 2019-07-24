#' Train
#'
#' Train a model using the given method.
#'
#' @param method character. Method used to train the model, possible values are
#' "Maxent" or "Maxnet".
#' @param data \code{\linkS4class{SWD}} object with presence and
#' absence/background locations.
#' @param folds matrix. Each column representing a fold with \code{TRUE} for
#' train locations and \code{FALSE} for test locations.
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
#' * For the Maxnet method, possible arguments are:
#'     + reg: numeric. The value of the regularization intensity, default is 1.
#'     + fc: vector. The value of the feature classes, possible values are
#'       combinations of "l", "q", "p", "h" and "t", default is "lqph". For more
#'       details see \code{\link[maxnet]{maxnet}}.
#'
#' @return An \code{\linkS4class{SDMmodel}} or \code{\linkS4class{SDMmodelCV}}
#' object.
#' @export
#' @importFrom progress progress_bar
#'
#' @seealso \code{\link{randomFolds}}
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
#' train <- prepareSWD(species = "Vultur gryphus", p = p_coords, a = bg_coords,
#'                     env = predictors, categorical = "biome")
#'
#' # Train a Maxent model
#' model <- train(method = "Maxent", data = train, fc = "l", reg = 1.5,
#'                iter = 700)
#'
#' # Train a Maxnet model
#' model <- train(method = "Maxnet", data = train, fc = "lq", reg = 1.5)
#'
#' # Train a Maxnet model with cross validation
#' model <- train(method = "Maxnet", data = train, fc = "l", reg = 0.8, rep = 4)
#' }
train <- function(method, data = NULL, folds = NULL, verbose = TRUE, p = NULL,
                  a = NULL, rep = NULL, seed = NULL, ...) {

  method <- match.arg(method, c("Maxent", "Maxnet"))
  f <- paste0("train", method)

  # TODO Remove in next release
  if (!is.null(p) & !is.null(a)) {
    stop("Argument \"p\" and \"a\" are deprecated, use \"data\" instead.")
  }
  if (!is.null(rep))
    warning("Argument \"rep\" is deprecated and will be removed in the nex ",
            "release. The number of partition is taken from the \"fold\" ",
            "argument.")

  if (is.null(folds)) {
    model <- do.call(f, args = list(data = data, ...))
  } else {
    k <- ncol(folds)
    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "Cross Validation [:bar] :percent in :elapsedfull",
        total = k, clear = FALSE, width = 60, show_after = 0)
      pb$tick(0)
    }

    models <- vector("list", length = k)

    # TODO Remove it next release
    if (!is.null(seed))
      warning("Argument seed is deprecated and will be removed in the next ",
              "release.", call. = FALSE)

    for (i in 1:k) {
      train <- .subset_swd(data, folds[, i])
      models[[i]] <- do.call(f, args = list(data = train, ...))
      if (verbose)
        pb$tick(1)
    }

    model <- SDMmodelCV(models = models, data = data, folds = folds)
  }

  return(model)
}

#' Create Random Folds
#'
#' Create random folds for cross validation
#'
#' @param data \code{\linkS4class{SWD}} object that will be used to train the
#' model.
#' @param k integer. Number of fold used to create the partition.
#' @param only_presence logical, if \code{TRUE} the random folds are created
#' only for the presence locations, used manly for presence-only methods,
#' default is \code{FALSE}.
#' @param seed integer. The value used to set the seed for the fold partition,
#' default is \code{NULL}.
#'
#' @return matrix with each column representing a fold. with \code{TRUE} for
#' train locations and \code{FALSE} for test locations.
#' @export
#'
#' @author Sergio Vignali
#' @examples
randomFolds <- function(data, k, only_presence = FALSE, seed = NULL) {

  if (class(data) != "SWD")
    stop("data argument is not of class SWD.")

  if (!is.null(seed))
    set.seed(seed)

  output <- matrix(TRUE, nrow = length(data@pa), ncol = k)
  p_folds <- cut(sample(1:nrow(.get_presence(data))), k, labels = FALSE)
  a_folds <- cut(sample(1:nrow(.get_absence(data))), k, labels = FALSE)

  for (i in 1:k) {
    if (only_presence) {
      output[, i] <- c(p_folds != k, rep(TRUE, nrow(.get_absence(data))))
    } else {
      folds <- c(p_folds, a_folds)
      output[, i] <- folds != k
    }
  }

  return(output)
}
