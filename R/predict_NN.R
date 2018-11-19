#' Predict NN
#'
#' Predict output for a new dataset from a trained Neural Network model.
#'
#' @param object NN object.
#' @param data data.frame containing values used for the prediction.
#' @param type character. Output type, at the moment only "logistic is
#' available.
#' @param clamp logical for clumping during prediction, default is TRUE.
#'
#' @include NN_class.R
#'
#' @return A vector with the prediction
#' @exportMethod predict
#' @importFrom keras %>%
#'
#' @examples \dontrun{
#' predict(model, my_dataset)}
#'
#' @author Sergio Vignali
setMethod("predict",
          signature = "NN",
          definition = function(object, data, type = "logistic", clamp = TRUE) {

            if (clamp) {
              for (var in object@min_max$variable) {
                data[var] <- clamp(data[, var],
                                   object@min_max$min[object@min_max$variable == var],
                                   object@min_max$max[object@min_max$variable == var])
              }
            }

            cat_vars <- names(object@levels)
            if (!is.null(cat_vars))
              data[cat_vars] <- lapply(data[cat_vars], factor)
            data <- format_data(data, object@means, object@stds, object@levels)
            data <- data.matrix(data)
            pred <- object@model %>% predict(data)

            return(as.vector(pred))
          })
