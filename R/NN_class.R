setOldClass("keras.engine.sequential.Sequential")
#' NN
#'
#' This Class represents a Neural Network model objects and hosts all the information
#' related to the model.
#'
#' @slot model keras.engine.sequential.Sequential. The Neural Network object.
#' @slot optimizer character. Name of optimizer algorithm used to train the
#' model.
#' @slot loss character. Name of the loss function optimized.
#' @slot means numeric. Vector of means used to standardize continuous
#' variables.
#' @slot stds numeric. Vector of standard deviations used to standardize
#' continuous variables.
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
                 model = "keras.engine.sequential.Sequential",
                 means = "numeric",
                 stds = "numeric")
)

setMethod("show",
          signature = "NN",
          definition = function(object) {
            cat("Class        :", class(object), "\n")
            cat("Layers       :", length(object@model$layers), "\n")
            cat("Loss function:", object@loss, "\n")
            cat("Optimizer    :", object@optimizer, "\n")
          })
