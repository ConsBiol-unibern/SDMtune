setOldClass("nnet")
#' Artificial Neural Network
#'
#' This Class represents an Artificial Neural Network model objects and hosts
#' all the information related to the model.
#'
#' @slot size integer. Number of the units in the hidden layer.
#' @slot decay numeric. Weight decay.
#' @slot rang numeric. Initial random weights.
#' @slot maxit integer. Maximum number of iterations.
#' @slot model \link{nnet}. The randomForest model object.
#'
#' @details See \link{nnet} for the meaning of the slots.
#'
#' @aliases NULL ANN-class
#' @export
#'
#' @author Sergio Vignali
ANN <- setClass("ANN",
                slots = c(
                  size = "numeric",
                  decay = "numeric",
                  rang = "numeric",
                  maxit = "numeric",
                  model = "nnet")
)

setMethod("show",
          signature = "ANN",
          definition = function(object) {
            cat("Class:", class(object), "\n")
            cat("size :", object@size, "\n")
            cat("decay:", object@decay, "\n")
            cat("rang :", object@rang, "\n")
            cat("maxit:", object@maxit, "\n")
          })
