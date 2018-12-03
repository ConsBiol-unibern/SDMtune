#' Train
#'
#' Train a model using the given method.
#'
#' @param method character. Possible values are "Maxent" or "Maxnet".
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param replicates numeric. Number of replicates, used for cross validation.
#' Default is 1, meaning no cross validation is performed.
#' @param verbose logical if TRUE shows a progress bar if replicates is greater
#' then 1, default is TRUE.
#' @param folds numeric. Vector containing the inxes for the k-fold partition of
#' the training data, if not provided the function uses the \link{k-fold}
#' function of the \link{dismo} package, default is NULL.
#' @param ... Arguments passed to the relative functions, see \link{trainMaxent}
#' or \link{trainMaxnet} for details related to the different methods.
#'
#' @return A SWDmodel or SDMmodelCV object.
#' @export
#' @importFrom dismo kfold
#' @importFrom progress progress_bar
#'
#' @examples \dontrun{
#' model <- train("Maxnet", presence, bg, reg = 2, fc = "lqp")}
#'
#' @author Sergio Vignali
train <- function(method = c("Maxent", "Maxnet"), presence, bg, replicates = 1,
                  verbose = TRUE, folds = NULL, ...) {
  method = match.arg(method)
  f <- paste0("train", method)

  if (replicates == 1) {
    model <- do.call(f, args = list(presence = presence, bg = bg, ...))
  } else {
    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "Cross Validation [:bar] :percent in :elapsedfull",
        total = replicates, clear = FALSE, width = 60, show_after = 0)
      pb$tick(0)
    }
    models <- vector("list", replicates)
    if (is.null(folds))
      folds <- dismo::kfold(presence@data, replicates)
    for (i in 1:replicates) {
      train <- presence
      train@data <- presence@data[folds != i, ]
      models[[i]] <- do.call(f, args = list(presence = train, bg = bg, ...))
      if (verbose)
        pb$tick(1)
    }
    model <- SDMmodelCV(models = models, presence = presence, folds = folds)
  }

  return(model)
}
