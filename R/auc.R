#' AUC
#'
#' Compute the AUC using the Man-Whitney U Test formula.
#'
#' @param model SDMmodel or SDMmodelCV object.
#' @param test SWD test locations for SDMmodel objects or TRUE for SDModelCV
#' objects, if not provided it computes the train AUC, default is NULL.
#' @param bg SWD backgroung locations used to compute the AUC
#' by the permutation importance function, defaul is NULL.
#'
#' @details If the model is a SDMmodelCV object, the function computes the mean
#' of the training or testing AUC values of the different replicates.
#'
#' @return The value of the AUC.
#' @export
#'
#' @examples
#' \dontrun{
#' auc(my_model, test = test_dataset)}
#'
#' @author Sergio Vignali
auc <- function(model, test = NULL, bg = NULL) {

  if (class(model) == "SDMmodel") {
    auc <- compute_auc(model, test, bg)
  } else {
    aucs <- c()
    for (i in 1:length(model@models)) {
      if (is.null(test)) {
        data <- model@presence
        data@data <- model@presence@data[model@folds != i, ]
      } else {
        data <- model@presence
        data@data <- model@presence@data[model@folds == i, ]
      }
      aucs <- c(aucs, compute_auc(model@models[[i]], data, bg))
    }
    auc <- mean(aucs)
  }

  return(round(auc, 4))
}

compute_auc <- function(model, test, bg) {

  if (class(model@model) == "Maxent") {
    type <- "raw"
  } else {
    type <- "link"
  }

  if (is.null(test)) {
    p_pred <- predict(model, model@presence@data, type = type)
  } else {
    p_pred <- predict(model, test@data[colnames(model@presence@data)],
                      type = type)
  }

  # bg is used for permutation importance
  if (is.null(bg)) {
    bg_pred <- predict(model, model@background@data, type = type)
  } else {
    bg_pred <- predict(model, bg@data, type = type)
  }

  n_pres <- length(p_pred)
  n_bg <- length(bg_pred)

  # AUC using the Mann-Whitney U Test (taken from dismo pkg)
  U <- sum(rank(c(p_pred, bg_pred))[seq(p_pred)]) - (n_pres * (n_pres + 1) / 2)
  auc <- U / (n_bg * n_pres)

  return(auc)
}
