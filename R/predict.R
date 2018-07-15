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
#' @param model A Maxent model object.
#' @param data A data frame, a \link[stack:raster] or \link{brick}.
#' @param clamp Flag for clumping during prediction, default is TRUE.
#' @param maxent_output The output type, possible values are "raw", "logistic" or "cloglog".
#' @param filename Output file name for the prediction map. If not provided the output is
#' saved in a file.
#' @param format The output format, see \link{writeRaster} for all the options, default is "GTiff".
#' @param extent \link{Extent} object, if provided it restricts the prediction to the given
#' extent, default is NULL.
#' @param parallel Flag to use parallel computation during prediction, default is FALSE.
#'
#' @details You need package \link{snow} to use parallel computation.
#'
#' @return A vector of prediction or a Raster object if data is a raster stack.
#'
#' @examples\dontrun{
#' predic(model, predictors, maxent_output = "cloglog", parallel = TRUE)}
#'
#' @author Sergio Vignali
predict.Maxent <- function(model, data, clamp = TRUE,
                           maxent_output = c("logistic", "cloglog", "raw"),
                           filename = "", format = "GTiff", extent = NULL,
                           parallel = FALSE) {

  start_time <- proc.time()
  maxent_output <- match.arg(maxent_output)

  if (inherits(data, "Raster")) {
    if (parallel) {
      beginCluster()
      p <- clusterR(data, predict, args = list(model = model,
                                               clamp = clamp,
                                               maxent_output = maxent_output,
                                               fun = .predict_from_lambdas),
                    progress = 'text', filename = filename, format = format,
                    ext = extent)
      endCluster()
    } else {
      p <- predict(data, model = model, maxent_output = maxent_output,
                   clamp = clamp, fun = .predict_from_lambdas, progress = 'text',
                   filename = filename, format = format, ext = extent)
    }
  } else if (inherits(data, "data.frame")) {
    p <- .predict_from_lambdas(model, data, maxent_output = maxent_output,
                               clamp = clamp)
  }

  elapsed_time <- proc.time() - start_time
  t_hour <- floor(elapsed_time[3] / 3600)
  t_min <- floor( (elapsed_time[3] - (t_hour * 3600)) / 60)
  t_sec <- elapsed_time[3] - (t_hour * 3600) - (t_min * 60)
  message(paste0("  - Prediction finished in ", t_hour, "h ", t_min, "m ",
                 round(t_sec, 1), "s"))
  return(p)
}
