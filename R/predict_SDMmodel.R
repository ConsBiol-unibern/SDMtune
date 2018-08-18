setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict
#'
#' Predict output for a new dataset from a trained SDMsel model.
#'
#' @param object SDMsel object.
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
#' @details You need package \pkg{snow} to use parallel computation and \pkg{rgdal}
#' to save the prediction in a raster file. Parallel computation increases the speed
#' only for big datasets due to the time necessary to create the cluster.
#' For **Maxent** models the function performs the prediction in **R** without
#' calling the **MaxEnt** java software. This results is a faster computation for
#' large datasets.
#'
#' @references Wilson P.D., (2009). Guidelines for computing MaxEnt model output
#' values from a lambdas file.
#'
#' @include Maxent_class.R Maxnet_class.R
#' @import methods
#' @importFrom raster beginCluster clusterR endCluster predict clamp
#' @importFrom stats formula model.matrix
#'
#' @return A vector with the prediction or a Raster object if data is a raster
#' stack/brick.
#' @exportMethod predict
#'
#' @examples\dontrun{
#' predict(model, predictors, parallel = TRUE)}
#'
#' @author Sergio Vignali
setMethod("predict",
          signature = "SDMmodel",
          definition = function(object, data, clamp = TRUE, filename = "",
                                format = "GTiff", extent = NULL,
                                parallel = FALSE, progress = "", type = NULL,
                                ...) {

            if (class(object@model) == "Maxent") {
              model <- object@model
            } else {
              model <- object@model@model
            }

            if (!is.null(type)) {
              type = type
            } else {
              type = object@type
            }

            if (inherits(data, "Raster")) {
              if (parallel) {
                suppressMessages(raster::beginCluster())
                pred <- raster::clusterR(data,
                                         predict,
                                         args = list(model = model,
                                                     clamp = clamp,
                                                     type = type,
                                                     fun = predict),
                                         progress = progress,
                                         filename = filename,
                                         format = format,
                                         ext = extent,
                                         ...)
                raster::endCluster()
              } else {
                pred <- raster::predict(data, model = model,
                                        type = type,
                                        clamp = clamp,
                                        fun = predict,
                                        progress = progress,
                                        filename = filename,
                                        format = format,
                                        ext = extent,
                                        ...)
              }
            } else if (inherits(data, "SWD")) {
              data <- data@data
              pred <- predict(model, data, type = type, clamp = clamp)
              pred <- as.vector(pred)
            } else if (inherits(data, "data.frame")) {
              pred <- predict(model, data, type = type, clamp = clamp)
              pred <- as.vector(pred)
            }

            return(pred)
          })
