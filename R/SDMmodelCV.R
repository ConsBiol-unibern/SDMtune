#' SDMmodelCV
#'
#' This Class represents a SDMmodel model object with replicates and hosts all
#' the models trained during the cross validation.
#'
#' @slot models list. A list containing all the models trained during the cross
#' validation.
#'
#' @name SDMmodelCV-class
#' @rdname SDMmodelCV-class
#' @exportClass SDMmodelCV
#'
#' @author Sergio Vignali
SDMmodelCV <- setClass("SDMmodelCV",
                     slots = c(models = "list")
)

setMethod("show",
          signature = "SDMmodelCV",
          definition = function(object) {
            cat("Class                :", class(object), "\n")
            cat("Model                :", class(object@models[[1]]@model), "\n")
            cat("Species              :", object@models[[1]]@presence@species, "\n")
            cat("Replicates           :", length(object@models), "\n")
            cat("Continuous variables :", names(Filter(is.numeric, object@models[[1]]@presence@data)), "\n")
            cat("Categorical variables:", names(Filter(is.factor, object@models[[1]]@presence@data)))
          }
)
