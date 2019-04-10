#' SDMmodelCV
#'
#' This Class represents a SDMmodel model object with replicates and hosts all
#' the models trained during the cross validation.
#'
#' @slot models list. A list containing all the models trained during the cross
#' validation.
#' @slot p \link{SWD} object. Full dataset used by the k-fold cross validation.
#' @slot a \link{SWD} object. The absence/background locations used to train the
#' model.
#' @slot folds numeric. Vector with the index for the k-fold partition.
#'
#' @include SWD_class.R
#' @name SDMmodelCV-class
#' @rdname SDMmodelCV-class
#' @exportClass SDMmodelCV
#'
#' @author Sergio Vignali
SDMmodelCV <- setClass("SDMmodelCV",
                       representation(
                         models = "list",
                         p = "SWD",
                         a = "SWD",
                         folds = "numeric"
                       )
)

setMethod(
  "show",
  signature = "SDMmodelCV",
  definition = function(object) {

    tunable_hypers <- get_tunable_args(object@models[[1]])
    cont_vars <- names(Filter(is.numeric, object@p@data))
    if (length(cont_vars) == 0)
      cont_vars <- NA
    cat_vars <- names(Filter(is.factor, object@p@data))
    if (length(cat_vars) == 0)
      cat_vars <- NA

    cat("Object of class", class(object), "\n")
    cat("Method:", class(object@models[[1]]@model), "\n\n")

    cat("Species:", object@p@species, "\n")
    cat("Replicates:", length(object@models), "\n")
    cat("Presence locations:", nrow(object@p@data), "\n\n")

    cat("Model configurations:\n")
    cat("--------------------\n")

    for (i in 1:length(tunable_hypers)) {
      if (tunable_hypers[i] == "a") {
        h <- nrow(object@a@data)
      } else {
        h <- slot(object@models[[1]]@model, tunable_hypers[i])
      }
      cat(tunable_hypers[i], ": ", h, "\n", sep = "")
    }

    cat("\nVariables:\n")
    cat("---------\n")
    cat("Continuous:", cont_vars, "\n")
    cat("Categorical:", cat_vars)
  }
)
