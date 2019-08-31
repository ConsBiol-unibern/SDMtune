setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict BRT
#'
#' Predict the output for a new dataset from a trained \linkS4class{BRT} model.
#'
#' @param object \code{\linkS4class{BRT}} object.
#' @param data data.frame with the data for the prediction.
#' @param type Not used.
#' @param clamp Not used.
#'
#' @details Used by the \code{\link{predict,SDMmodel-method}}, not exported.
#'
#' The function uses the number of tree defined to train the model and the
#' "response" type output.
#'
#' @include BRT-class.R
#'
#' @return A vector with the predicted values.
#'
#' @author Sergio Vignali
setMethod("predict",
          signature = "BRT",
          definition = function(object, data, type, clamp) {
            output <- predict(object@model, newdata = data,
                              n.trees = object@n.trees, type = "response")
            return(output)
          })
