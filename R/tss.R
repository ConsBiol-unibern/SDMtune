#' True Skill Statistics
#'
#' Compute the max TSS of a given model.
#'
#' @param model SDMmodel or SDMmodelCV object.
#' @param test SWD test locations for SDMmodel objects or logical for SDModelCV
#' objects, if not provided it computes the train AUC, default is NULL.
#'
#' @details If the model is a SDMmodelCV object, the function computes the mean
#' of the training or testing TSS values of the different replicates.
#'
#' @return The value of the TSS of the given model.
#' @export
#'
#' @examples \dontrun{
#' tss(model, test = test_dataset)}
#'
#' @author Sergio Vignali
#'
#' @references
#' Allouche O., Tsoar A., Kadmon R., (2006). Assessing the accuracy of species distribution models:
#' prevalence, kappa and the true skill statistic (TSS). Journal of Applied Ecology, 43(6), 1223–1232.
tss <- function(model, test = NULL) {

  if (class(model) == "SDMmodel") {
    tss <- max(compute_tss(model, test))
  } else {
    tsss <- c()
    for (i in 1:length(model@models)) {
      if (is.null(test)) {
        data <- model@presence
        data@data <- model@presence@data[model@folds != i,  , drop = FALSE]
      } else {
        data <- model@presence
        data@data <- model@presence@data[model@folds == i,  , drop = FALSE]
      }
      tsss <- c(tsss, max(compute_tss(model@models[[i]], data)))
    }
    tss <- mean(tsss)
  }

  return(round(tss, 4))
}

compute_tss <- function(model, test) {

  cm <- confMatrix(model, test = test, type = "logistic")
  tpr <- cm$tp / (cm$tp + cm$fn)
  tnr <- cm$tn / (cm$fp + cm$tn)
  tss <- tpr + tnr - 1

  return(tss)
}
