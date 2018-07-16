setGeneric("predict", function(object, ...)
    standardGeneric("predict")
    )

.predict_from_lambdas <- function(model, data, maxent_output, clamp = TRUE) {

    if (clamp) {
      for (variable in model@min_max$variable) {
        data[variable] <- clamp(data[, variable],
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
    if (maxent_output == "raw") {
      return(raw)
    } else if (maxent_output == "logistic") {
       return(raw * exp(model@entropy) / (1 + raw * exp(model@entropy)))
    } else {
      return(raw * exp(model@entropy) / (1 + raw * exp(model@entropy)))
    }
}

#' Predict Maxent
#'
#' @param object Maxent object.
#' @param data data.frame, \code{\link{stack}} or \code{\link{brick}}.
#' @param clamp logical for clumping during prediction, default is TRUE.
#' @param maxent_output character. The Maxent output type, possible values are "raw", "logistic" or "cloglog".
#' @param filename character. Output file name for the prediction map, if provided the output is
#' saved in a file.
#' @param format character. The output format, see \code{\link{writeRaster}} for all the options, default is "GTiff".
#' @param extent \link{Extent} object, if provided it restricts the prediction to the given
#' extent, default is NULL.
#' @param parallel logical to use parallel computation during prediction, default is FALSE.
#' @param progress character to display a progress bar: "text", "window" or "" (default) for no progress bar.
#' @param ... Additional parameter to pass to the \code{\link{writeRaster}} function.
#'
#' @details You need package \link{snow} to use parallel computation and \code{\link{rgdal}} to save the prediction in a raster file.
#' @include Maxent_class.R
#' @importFrom raster beginCluster clusterR endCluster predict
#'
#' @return A vector of prediction or a Raster object if data is a raster stack/brick.
#' @exportMethod predict
#'
#' @examples\dontrun{
#' predict(model, predictors, maxent_output = "cloglog", parallel = TRUE)}
#'
#' @author Sergio Vignali
setMethod("predict",
          signature = "Maxent",
          definition = function(object, data, clamp = TRUE,
                                maxent_output = c("logistic", "cloglog", "raw"),
                                filename = "", format = "GTiff", extent = NULL,
                                parallel = FALSE, progress = "", ...) {
            start_time <- proc.time()
            maxent_output <- match.arg(maxent_output)

            if (inherits(data, "Raster")) {
              if (parallel) {
                raster::beginCluster()
                pred <- raster::clusterR(data,
                                         predict,
                                         args = list(model = object,
                                                     clamp = clamp,
                                                     maxent_output = maxent_output,
                                                     fun = .predict_from_lambdas),
                                         progress = progress,
                                         filename = filename,
                                         format = format,
                                         ext = extent,
                                         ...)
                raster::endCluster()
              } else {
                pred <- raster::predict(data, model = object,
                                        maxent_output = maxent_output,
                                        clamp = clamp,
                                        fun = .predict_from_lambdas,
                                        progress = progress,
                                        filename = filename,
                                        format = format,
                                        ext = extent,
                                        ...)
              }
            } else if (inherits(data, "data.frame")) {
              pred <- .predict_from_lambdas(object,
                                            data,
                                            maxent_output = maxent_output,
                                            clamp = clamp)
            }

            elapsed_time <- proc.time() - start_time
            t_hour <- floor(elapsed_time[3] / 3600)
            t_min <- floor( (elapsed_time[3] - (t_hour * 3600)) / 60)
            t_sec <- elapsed_time[3] - (t_hour * 3600) - (t_min * 60)
            message(paste0("  - Prediction finished in ", t_hour, "h ", t_min,
                           "m ", round(t_sec, 1), "s"))
            return(pred)
          })
