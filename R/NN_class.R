setOldClass(c("keras.engine.sequential.Sequential", "keras_training_history"))
#' NN
#'
#' This Class represents a Neural Network model objects and hosts all the information
#' related to the model.
#'
#' @slot model keras.engine.sequential.Sequential. The Neural Network object.
#' @slot loss character. Name of the loss function optimized.
#' @slot optimizer character. Name of optimizer algorithm used to train the
#' model.
#' @slot min_max data.frame. The minimum and maximum values of the continuous
#' variables, used for clamping.
#' @slot means numeric. Vector of means used to standardize continuous
#' variables.
#' @slot stds numeric. Vector of standard deviations used to standardize
#' continuous variables.
#' @slot levels list with the levels of each categorical variable.
#'
#' @name NN-class
#' @rdname NN-class
#' @exportClass NN
#'
#' @author Sergio Vignali
NN <- setClass("NN",
               representation(
                 model = "keras.engine.sequential.Sequential",
                 history = "keras_training_history",
                 loss = "character",
                 optimizer = "character",
                 min_max = "data.frame",
                 means = "numeric",
                 stds = "numeric",
                 levels = "list")
)

setMethod("show",
          signature = "NN",
          definition = function(object) {
            cat("Class        :", class(object), "\n")
            cat("Layers       :", length(object@model$layers), "\n")
            cat("Loss function:", object@loss, "\n")
            cat("Optimizer    :", object@optimizer, "\n")
          })
