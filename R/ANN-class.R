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
            cli::cli_h2("Object of class: {.cls {class(object)}}")

            cli::cli_par()
            cli::cli_text("Method: {.emph Artificial Neural Network}")
            cli::cli_end()

            cli::cli_par()
            cli::cli_h3("Hyperparameters")
            cli::cli_end()

            cli::cli_par()
            cli::cli_li("{.field size}: {object@size}")
            cli::cli_li("{.field decay}: {object@decay}")
            cli::cli_li("{.field rang}: {object@rang}")
            cli::cli_li("{.field maxit}: {object@maxit}")
            cli::cli_end()
          })
