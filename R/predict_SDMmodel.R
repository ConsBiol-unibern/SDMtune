setGeneric("predict", function(object, ...)
  standardGeneric("predict")
)

#' Predict
#'
#' Predict output for a new dataset from a trained model.
#'
#' @param object SDMmodel object.
#' @param data data.frame, \link{SWD}, \link{stack}.
#' @param type character. Output type, see \link{predict,Maxent-method} for
#' Maxent models or \link{predict.maxnet} for Maxnet models.
#' @param clamp logical for clumping during prediction, default is TRUE.
#' @param filename character. Output file name for the prediction map, if
#' provided the output is saved in a file.
#' @param format character. The output format, see \link{writeRaster} for all
#' the options, default is "GTiff".
#' @param extent \link{Extent} object, if provided it restricts the prediction
#' to the given extent, default is NULL.
#' @param parallel logical to use parallel computation during prediction,
#' default is FALSE.
#' @param progress character to display a progress bar: "text", "window" or ""
#' (default) for no progress bar.
#' @param ... Additional arguments to pass to the \link{writeRaster} function.
#'
#' @details You need package \pkg{snow} to use parallel computation and
#' \pkg{rgdal} to save the prediction in a raster file. Parallel computation
#' increases the speed only for big datasets due to the time necessary to create
#' the cluster. For **Maxent** models the function performs the prediction in
#' **R** without calling the **MaxEnt** java software. This results is a faster
#' computation for large datasets.
#'
#' @references Wilson P.D., (2009). Guidelines for computing MaxEnt model output
#' values from a lambdas file.
#'
#' @include Maxent_class.R Maxnet_class.R
#' @import methods
#' @importFrom raster beginCluster clusterR endCluster predict clamp subset
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
          definition = function(object, data, type, clamp = TRUE, filename = "",
                                format = "GTiff", extent = NULL,
                                parallel = FALSE, progress = "", ...) {

            if (class(object@model) != "Maxnet") {
              model <- object@model
            } else {
              model <- object@model@model
            }

            vars <- colnames(object@presence@data)

            if (inherits(data, "Raster")) {
              data <- raster::subset(data, vars)
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
              data <- data@data[vars]
              pred <- predict(model, data, type = type, clamp = clamp)
              pred <- as.vector(pred)
            } else if (inherits(data, "data.frame")) {
              data <- data[vars]
              pred <- predict(model, data, type = type, clamp = clamp)
              pred <- as.vector(pred)
            }

            return(pred)
          })
