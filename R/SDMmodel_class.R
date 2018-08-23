setClassUnion("model", c("Maxent", "Maxnet"))
#' SDMmodel
#'
#' This Class represents a MaxEnt model objects and hosts all the information related to the model.
#'
#' @slot presence SWD. The presence locations used to train the model.
#' @slot background SWD. The backgorund locations used to train the model.
#' @slot model Maxent or Maxnet object.
#' @slot html character. The path of the html file, available only after running
#' the function \link{modelReport}.
#' runMaxent function.
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

setMethod("show",
  signature = "SDMmodel",
  definition = function(object) {
    cat("Class                :", class(object), "\n")
    cat("Model                :", class(object@model), "\n")
    cat("Species              :", object@presence@species, "\n")
    cat("Presence data        :", nrow(object@presence@data), "\n")
    cat("Background data      :", nrow(object@background@data), "\n")
    cat("Continuous variables :", names(Filter(is.numeric, object@presence@data)), "\n")
    cat("Categorical variables:", names(Filter(is.factor, object@presence@data)))

    if (!identical(object@html, character(0)))
      browseURL(object@html)
  }
)
