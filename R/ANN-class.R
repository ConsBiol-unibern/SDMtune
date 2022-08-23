setOldClass("nnet")
#' Artificial Neural Network
#'
#' This Class represents an Artificial Neural Network model object and hosts
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
#' @rdname ANN-class
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

#' @param object ANN object
#' @rdname ANN-class
setMethod("show",
          signature = "ANN",
          definition = function(object) {
            cat("Class:", class(object), "\n")
            cat("size :", object@size, "\n")
            cat("decay:", object@decay, "\n")
            cat("rang :", object@rang, "\n")
            cat("maxit:", object@maxit, "\n")
          })
