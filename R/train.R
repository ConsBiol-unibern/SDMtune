#' Train
#'
#' Train a model using the given method.
#'
#' @param method character. Possible values are "Maxent" or "Maxnet".
#' @param presence SWD object with the presence locations.
#' @param bg SWD object with the background locations.
#' @param replicates numeric. Number of replicates, used for cross validation.
#' Default is 1, meaning no cross validation is performed.
#' @param ... Arguments passed to the relative functions, see \link{trainMaxent}
#' or \link{trainMaxnet} for details related to the different methods.
#'
#' @return A SWDmodel or SDMmodelCV object.
#' @export
#' @importFrom dismo kfold
#'
#' @examples \dontrun{
#' model <- train("Maxnet", presence, bg, reg = 2, fc = "lqp")}
#'
#' @author Sergio Vignali
train <- function(method = c("Maxent", "Maxnet"), presence, bg, replicates = 1,
                  ...) {
  method = match.arg(method)
  f <- paste0("train", method)

  if (replicates == 1) {
    model <- do.call(f, args = list(presence = presence, bg = bg, ...))
  } else {
    pb <- progress::progress_bar$new(
      format = "Cross Validation [:bar] :percent in :elapsedfull",
      total = replicates, clear = FALSE, width = 60, show_after = 0)
    pb$tick(0)
    models <- vector("list", replicates)
    folds <- dismo::kfold(presence@data, replicates)
    for (i in 1:replicates) {
      train <- presence
      train@data <- presence@data[folds != i, ]
      models[[i]] <- do.call(f, args = list(presence = train, bg = bg, ...))
      pb$tick(1)
    }
    model <- SDMmodelCV(models = models, presence = presence)
  }

  return(model)
}
