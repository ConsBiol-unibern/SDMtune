setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

setMethod("predict",
          signature = "RF",
          definition = function(object, data, type, clamp) {
            output <- predict(object@model, data, type = "prob")[, 2]
            return(output)
          })
