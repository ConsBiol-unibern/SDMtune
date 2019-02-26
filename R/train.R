#' Train
#'
#' Train a model using the given method.
#'
#' @param method character. Possible values are "Maxent" or "Maxnet".
#' @param p \link{SWD} object with the presence locations.
#' @param a \link{SWD} object with the absence or background locations.
#' @param rep numeric. Number of replicates, used for cross validation.
#' Default is 1, meaning no cross validation is performed.
#' @param verbose logical if TRUE shows a progress bar if replicates is greater
#' then 1, default is TRUE.
#' @param folds numeric. Vector containing the indexes for the k-fold partition
#' of the training data, if not provided the function randomly creates the
#' folds, default is NULL.
#' @param seed integer. The value used to set the seed for the fold partition,
#' used if **folds** is not provided, default is NULL.
#' @param ... Arguments passed to the relative functions, see \link{trainMaxent}
#' or \link{trainMaxnet} for details related to the different methods.
#'
#' @return A \link{SWDmodel} or \link{SDMmodelCV} object.
#' @export
#' @importFrom dismo kfold
#' @importFrom progress progress_bar
#'
#' @examples \dontrun{
#' model <- train("Maxnet", p, a, reg = 2, fc = "lqp")}
#'
#' @author Sergio Vignali
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
