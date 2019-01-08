#' SDMmodelCV
#'
#' This Class represents a SDMmodel model object with replicates and hosts all
#' the models trained during the cross validation.
#'
#' @slot models list. A list containing all the models trained during the cross
#' validation.
#' @slot presence SWD object. Full dataset used by the k-fold cross validation.
#' @slot background SWD. The backgorund locations used to train the model.
#' @slot folds numeric. Vector with the index of the k-fold.
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
                         presence = "SWD",
                         background = "SWD",
                         folds = "numeric"
                       )
)

setMethod("show",
          signature = "SDMmodelCV",
          definition = function(object) {
            cat("Class                :", class(object), "\n")
            cat("Model                :", class(object@models[[1]]@model), "\n")
            cat("Species              :", object@models[[1]]@presence@species,
                "\n")
            cat("Presence data        :", nrow(object@presence@data), "\n")
            cat("Background data      :", nrow(object@background@data), "\n")
            cat("Reg                  :", object@models[[1]]@model@reg, "\n")
            cat("FC                   :", object@models[[1]]@model@fc, "\n")
            cat("Replicates           :", length(object@models), "\n")
            cat("Continuous variables :",
                names(Filter(is.numeric, object@models[[1]]@presence@data)),
                "\n")
            cat("Categorical variables:",
                names(Filter(is.factor, object@models[[1]]@presence@data)))
          }
)
