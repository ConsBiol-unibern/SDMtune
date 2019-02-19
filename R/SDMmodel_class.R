setClassUnion("model", c("Maxent", "Maxnet"))
#' SDMmodel
#'
#' This Class represents a SDMmodel model object and hosts all the information
#' related to the model.
#'
#' @slot presence \link{SWD} object. The presence locations used to train the
#' model.
#' @slot background \link{SWD} object. The backgorund locations used to train
#' the model.
#' @slot model \link{Maxent} or \link{Maxnet} object.
#' @slot html character. The path of the html file, available only after running
#' the function \link{modelReport}.
#'
#' @include SWD_class.R Maxent_class.R Maxnet_class.R
#' @name SDMmodel-class
#' @rdname SDMmodel-class
#' @exportClass SDMmodel
#'
#' @author Sergio Vignali
SDMmodel <- setClass("SDMmodel",
  representation(
    presence = "SWD",
    background = "SWD",
    model = "model",
    html = "character"),
  validity = function(object)	{
    if (ncol(object@presence@data) != ncol(object@background@data))
      return("presence and background have a different number of columns!")

    if (length(setdiff(sort(colnames(object@presence@data)),
                            sort(colnames(object@background@data)))) > 0)
      return("presence and background have different variables!")

    return(TRUE)
 }
)

setMethod(
  "show",
  signature = "SDMmodel",
  definition = function(object) {

    cont_vars <- names(Filter(is.numeric, object@presence@data))
    if (length(cont_vars) == 0)
      cont_vars <- NA
    cat_vars <- names(Filter(is.factor, object@presence@data))
    if (length(cat_vars) == 0)
      cat_vars <- NA

    cat("Object of class", class(object), "\n")
    cat("Method:", class(object@model), "\n\n")

    cat("Species:", object@presence@species, "\n")
    cat("Presence locations:", nrow(object@presence@data), "\n\n")

    cat("Model hyperparameters:\n")
    cat("---------------------\n")
    cat("Reg         :", object@model@reg, "\n")
    cat("FC          :", object@model@fc, "\n")
    cat("Bg locations:", nrow(object@background@data), "\n\n")

    cat("Variables:\n")
    cat("---------\n")
    cat("Continuous:", cont_vars, "\n")
    cat("Categorical:", cat_vars)

    if (!identical(object@html, character(0)))
      browseURL(object@html)
  }
)
