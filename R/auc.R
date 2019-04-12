#' AUC
#'
#' Compute the AUC using the Man-Whitney U Test formula.
#'
#' @param model \link{SDMmodel} or \link{SDMmodelCV} object.
#' @param test \link{SWD} test locations for \link{SDMmodel} objects or logical
#' for \link{SDMmodelCV} objects, if not provided it computes the train AUC,
#' default is NULL.
#' @param a \link{SWD} absence or background locations used to compute the AUC
#' by the permutation importance function, default is NULL.
#'
#' @details If the model is a \link{SDMmodelCV} object, the function computes
#' the mean of the training or testing AUC values of the different replicates.
#'
#' @return The value of the AUC.
#' @export
#' @importFrom stats wilcox.test
#'
#' @examples
#' \dontrun{
#' auc(my_model, test = test_dataset)}
#'
#' @author Sergio Vignali
auc <- function(model, test = NULL, a = NULL) {

  if (class(model) == "SDMmodel") {
    auc <- .compute_auc(model, test, a)
  } else {
    aucs <- c()
    for (i in 1:length(model@models)) {
      if (is.null(test)) {
        data <- model@p
        data@data <- model@p@data[model@folds != i, , drop = FALSE]
      } else {
        data <- model@p
        data@data <- model@p@data[model@folds == i, , drop = FALSE]
      }
      aucs <- c(aucs, .compute_auc(model@models[[i]], data, a))
    }
    auc <- mean(aucs)
  }

  return(round(auc, 4))
}

.compute_auc <- function(model, test, a) {

  if (class(model@model) == "Maxent") {
    type <- "raw"
  } else {
    type <- "link"
  }

  if (is.null(test)) {
    p_pred <- predict(model, model@p@data, type = type)
  } else {
    p_pred <- predict(model, test@data[colnames(model@p@data)],
                      type = type)
  }

  # a is used for permutation importance
  if (is.null(a)) {
    a_pred <- predict(model, model@a@data, type = type)
  } else {
    a_pred <- predict(model, a@data, type = type)
  }

  # AUC using the Mann-Whitney U Test (inspired by dismo pkg)
  U <- as.numeric(wilcox.test(p_pred, a_pred)$statistic)
  auc <- U / (length(p_pred) * length(a_pred))

  return(auc)
}
