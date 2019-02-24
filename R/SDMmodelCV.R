#' SDMmodelCV
#'
#' This Class represents a SDMmodel model object with replicates and hosts all
#' the models trained during the cross validation.
#'
#' @slot models list. A list containing all the models trained during the cross
#' validation.
#' @slot p \link{SWD} object. Full dataset used by the k-fold cross validation.
#' @slot a \link{SWD} object. The absence/backgorund locations used to train the
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

    cat("Model hyperparameters:\n")
    cat("---------------------\n")
    cat("Reg         :", object@models[[1]]@model@reg, "\n")
    cat("FC          :", object@models[[1]]@model@fc, "\n")
    cat("Bg locations:", nrow(object@a@data), "\n\n")

    cat("Variables:\n")
    cat("---------\n")
    cat("Continuous:", cont_vars, "\n")
    cat("Categorical:", cat_vars)
  }
)
