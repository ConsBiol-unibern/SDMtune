setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict Maxnet
#'
#' Predict the output for a new dataset from a trained Maxnet model.
#'
#' @param object \linkS4class{Maxnet} object.
#' @param data data.frame with the data for the prediction.
#' @param type character. Maxnet output type, possible values are "link",
#'  "exponential", "cloglog" and "logistic".
#' @param clamp logical for clumping during prediction.
#'
#' @details Used by the \link{predict,SDMmodel-method}, not exported.
#'
#' @include Maxnet-class.R
#'
#' @return A vector with the predicted values.
#'
#' @author Sergio Vignali
setMethod(
  f = "predict",
  signature = "Maxnet",
  definition = function(object,
                        data,
                        type = c("link", "exponential", "cloglog", "logistic"),
                        clamp = TRUE) {

    predict(object@model,
            newdata = data,
            type = type,
            clamp = clamp)
  }
)
