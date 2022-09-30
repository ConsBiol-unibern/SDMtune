setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict RF
#'
#' Predict the output for a new dataset from a trained RF model.
#'
#' @param object \linkS4class{RF} object.
#' @param data data.frame with the data for the prediction.
#' @param type Not used.
#' @param clamp Not used.
#'
#' @details Used by the \link{predict,SDMmodel-method}, not exported.
#'
#' @include RF-class.R
#'
#' @return A vector with the predicted probabilities of class 1.
#'
#' @author Sergio Vignali
setMethod(
  f = "predict",
  signature = "RF",
  definition = function(object,
                        data,
                        type,
                        clamp) {

    predict(object@model,
            data,
            type = "prob")[, 2]
  }
)
