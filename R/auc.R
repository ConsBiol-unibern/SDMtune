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
#' @references Mason, S. J. and Graham, N. E. (2002), Areas beneath the relative
#' operating characteristics (ROC) and relative operating levels (ROL) curves:
#' Statistical significance and interpretation. Q.J.R. Meteorol. Soc., 128:
#' 2145-2166.
#'
#' @return The value of the AUC.
#' @export
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
    aucs <- vector("numeric", length = length(model@models))
    data <- model@p
    for (i in 1:length(model@models)) {
      if (is.null(test)) {
        data@data <- model@p@data[model@folds != i, , drop = FALSE]
      } else {
        data@data <- model@p@data[model@folds == i, , drop = FALSE]
      }
      aucs[i] <- .compute_auc(model@models[[i]], data, a)
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
    p <- model@p@data
  } else {
    p <- test@data[colnames(model@p@data)]
  }

  # a is used for permutation importance
  if (is.null(a)) {
    a <- model@a@data
  } else {
    a <- a@data[colnames(model@p@data)]
  }

  pred <- predict(model, rbind(p, a), type = type)
  n_p <- nrow(p)
  n_a <- nrow(a)

  # AUC using the Mann-Whitney U Test
  Rp <- sum(rank(pred)[1:n_p])  # Sum of rank of positive cases
  Up <- Rp - (n_p * (n_p + 1) / 2)  # U test for positive cases
  auc <- Up / (n_p * n_a)

  return(auc)
}
