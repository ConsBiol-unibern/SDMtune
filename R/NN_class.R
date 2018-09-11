setOldClass("keras.engine.sequential.Sequential")
#' NN
#'
#' This Class represents a Neural Network model objects and hosts all the information
#' related to the model.
#'
#' @slot model keras.engine.sequential.Sequential. The Neural Network object.
#'
#' @name NN-class
#' @rdname NN-class
#' @exportClass NN
#'
#' @author Sergio Vignali
NN <- setClass("NN",
               slots = c(
                 optimizer = "character",
                 loss = "character",
                 model = "keras.engine.sequential.Sequential")
)

setMethod("show",
          signature = "NN",
          definition = function(object) {
            cat("Class        :", class(object), "\n")
            cat("Layers       :", length(object@model$layers), "\n")
            cat("Loss function:", object@loss, "\n")
            cat("Optimizer    :", object@optimizer, "\n")
          })
