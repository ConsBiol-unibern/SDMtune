setGeneric("predict", function(object, ...)
    standardGeneric("predict")
    )

.predict_from_lambdas <- function(model, data, type, clamp) {

    if (clamp) {
      for (variable in model@min_max$variable) {
        data[variable] <- raster::clamp(data[, variable],
                                        model@min_max$min[model@min_max$variable == variable],
                                        model@min_max$max[model@min_max$variable == variable])
      }
    }

    f <- model@formula
    # Make the design matrix
    dm <- model.matrix(f, data)
    # Clamp feature if outside fo the scaling normalization
    if (clamp) dm <- t(pmin(pmax(t(dm), 0), 1))

    S <- (dm %*% model@coeff$lambda) - model@lpn
    raw <- exp(S) / model@dn
    if (type == "raw") {
      return(raw)
    } else if (type == "logistic") {
       return(raw * exp(model@entropy) / (1 + raw * exp(model@entropy)))
    } else {
      return(1 - exp(-raw * exp(model@entropy)))
    }
}

#' Predict Maxent
#'
#' @param object Maxent object.
#' @param data data.frame, \link{SWD}, \link{stack} or \link{brick}.
#' @param clamp logical for clumping during prediction, default is TRUE.
#' @param filename character. Output file name for the prediction map, if provided the output is
#' saved in a file.
#' @param format character. The output format, see \link{writeRaster} for all the options, default is "GTiff".
#' @param extent \link{Extent} object, if provided it restricts the prediction to the given
#' extent, default is NULL.
#' @param parallel logical to use parallel computation during prediction, default is FALSE.
#' @param progress character to display a progress bar: "text", "window" or "" (default) for no progress bar.
#' @param type character MaxEnt output type, if not provided it uses the model type.
#' Possible values are "cloglog", "logistic" and "raw", default is NULL.
#' @param ... Additional parameter to pass to the \link{writeRaster} function.
#'
#' @details You need package \link{snow} to use parallel computation and \link{rgdal}
#' to save the prediction in a raster file. Parallel computation increases the speed
#' only for big datasets due to the time necessary to create the cluster.
#' @include Maxent_class.R
#' @importFrom raster beginCluster clusterR endCluster predict clamp
#'
#' @return A vector of prediction or a Raster object if data is a raster stack/brick.
#' @exportMethod predict
#' @rdname predict
#'
#' @examples\dontrun{
#' predict(model, predictors, parallel = TRUE)}
#'
#' @author Sergio Vignali
setMethod("predict",
          signature = "Maxent",
          definition = function(object, data, clamp = TRUE, filename = "",
                                format = "GTiff", extent = NULL,
                                parallel = FALSE, progress = "", type = NULL,
                                ...) {

            if (!is.null(type)) {
              type = type
            } else {
              type = object@type
            }

            if (inherits(data, "Raster")) {
              if (parallel) {
                raster::beginCluster()
                pred <- raster::clusterR(data,
                                         predict,
                                         args = list(model = object,
                                                     clamp = clamp,
                                                     type = type,
                                                     fun = .predict_from_lambdas),
                                         progress = progress,
                                         filename = filename,
                                         format = format,
                                         ext = extent,
                                         ...)
                raster::endCluster()
              } else {
                pred <- raster::predict(data, model = object,
                                        type = type,
                                        clamp = clamp,
                                        fun = .predict_from_lambdas,
                                        progress = progress,
                                        filename = filename,
                                        format = format,
                                        ext = extent,
                                        ...)
              }
            } else if (inherits(data, "SWD")) {
              data <- data@data
              pred <- .predict_from_lambdas(object,
                                            data,
                                            type = type,
                                            clamp = clamp)
              pred <- as.vector(pred)
            } else if (inherits(data, "data.frame")) {
              pred <- .predict_from_lambdas(object,
                                            data,
                                            type = type,
                                            clamp = clamp)
              pred <- as.vector(pred)
            }

            return(pred)
          })
