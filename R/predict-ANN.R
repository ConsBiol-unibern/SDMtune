setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict ANN
#'
#' Predict the output for a new dataset from a trained \linkS4class{ANN} model.
#'
#' @param object \code{\linkS4class{BRT}} object.
#' @param data data.frame with the data for the prediction.
#' @param type Not used.
#' @param clamp Not used.
#'
#' @details Used by the \code{\link{predict,SDMmodel-method}}, not exported.
#'
#' @include ANN-class.R
#'
#' @return A vector with the predicted values.
#'
#' @author Sergio Vignali
setMethod("predict",
          signature = "ANN",
          definition = function(object, data, type, clamp) {
            output <- predict(object@model, newdata = data, type = "raw")
            return(output)
          })
