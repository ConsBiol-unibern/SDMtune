setGeneric("predict", function(object, ...)
    standardGeneric("predict")
    )

#' Predict Maxent
#'
#' Predict output for a new dataset from a trained Maxent model.
#'
#' @param Maxent object.
#' @param data data.frame containing values used for the prediction.
#' @param type character MaxEnt output type, if not provided it uses the model
#' type. Possible values are "cloglog", "logistic" and "raw", default is NULL.
#' @param clamp logical for clumping during prediction, default is TRUE.
#'
#' @details The function performs the prediction in **R** without calling the
#' **MaxEnt** java software. This results is a faster computation for large
#' datasets.
#'
#' @references Wilson P.D., (2009). Guidelines for computing MaxEnt model output
#' values from a lambdas file.
#'
#' @include Maxent_class.R
#'
#' @return A vector with the prediction
#' @exportMethod predict
#'
#' @examples \dontrun{
#' predict(model, my_dataset, type = "cloglog")}
#'
#' @author Sergio Vignali
setMethod("predict",
          signature = "Maxent",
          definition = function(object, data,
                                type = c("cloglog", "logistic", "raw"),
                                clamp = TRUE) {

    type <- match.arg(type)

    if (clamp) {
      for (variable in object@min_max$variable) {
        data[variable] <- raster::clamp(data[, variable],
                                        object@min_max$min[object@min_max$variable == variable],
                                        object@min_max$max[object@min_max$variable == variable])
      }
    }

    f <- object@formula
    # Make the design matrix
    dm <- model.matrix(f, data)
    # Clamp feature if outside fo the scaling normalization
    if (clamp) dm <- t(pmin(pmax(t(dm), 0), 1))

    S <- (dm %*% object@coeff$lambda) - object@lpn
    raw <- exp(S) / object@dn
    raw[raw == Inf] <- 1
    if (type == "raw") {
      return(raw)
    } else if (type == "logistic") {
       return(raw * exp(object@entropy) / (1 + raw * exp(object@entropy)))
    } else {
      return(1 - exp(-raw * exp(object@entropy)))
    }
})
