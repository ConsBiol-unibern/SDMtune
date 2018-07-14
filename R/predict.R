.predict_from_lambdas <- function(model, data, output_format, clamp = TRUE) {

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
    if (output_format == "raw") {
      return(raw)
    } else if (output_format == "logistic") {
       return(raw * exp(model@entropy) / (1 + raw * exp(model@entropy)))
    } else {
      return(raw * exp(model@entropy) / (1 + raw * exp(model@entropy)))
    }
}

#' Predict Maxent
#'
#' @param model A Maxent model object.
#' @param data A data frame or a raster Stack.
#' @param clamp Flag for doing clumping during prediction, default is TRUE.
#' @param parallel Flag to use parallel computation during prediction, default is FALSE.
#' @param output_format The format of the output, possible values are "raw", "logistic" or "cloglog".
#'
#' @details You need package snow to use parallel computation.
#'
#' @return A vector of prediction or a Raster object if data is a raster stack.
#'
#' @examples\dontrun{
#' predic(model, predictors, output_format = "cloglog", parallel = TRUE)}
#'
#' @author Sergio Vignali
predict.Maxent <- function(model, data, clamp = TRUE, parallel = FALSE,
                           output_format = c("logistic", "cloglog", "raw")) {

  start_time <- proc.time()
  format <- match.arg(output_format)

  if (inherits(data, "Raster")) {
    if (parallel) {
      beginCluster()
      p <- clusterR(data, predict, args = list(model = model,  clamp = clamp,
                                              output_format = format,
                                              fun = .predict_from_lambdas,
                                              progress = 'text'))
      endCluster()
    } else {
      p <- predict(data, model = model, output_format = format, clamp = clamp,
                   fun = .predict_from_lambdas, progress = 'text')
    }
  } else if (inherits(data, "data.frame")) {
    p <- .predict_from_lambdas(model, data, output_format = format,
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
