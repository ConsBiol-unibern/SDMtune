setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict ANN
#'
#' Predict the output for a new dataset from a trained ANN model.
#'
#' @param object \linkS4class{ANN} object.
#' @param data data.frame with the data for the prediction.
#' @param type Not used.
#' @param clamp Not used.
#'
#' @details Used by the \link{predict,SDMmodel-method}, not exported.
#'
#' @include ANN-class.R
#'
#' @return A vector with the predicted values.
#'
#' @author Sergio Vignali
setMethod(
  f = "predict",
  signature = "ANN",
  definition = function(object,
                        data,
                        type,
                        clamp) {

    predict(object@model,
            newdata = data,
            type = "raw")
  }
)
