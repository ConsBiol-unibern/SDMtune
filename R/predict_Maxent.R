setGeneric("predict", function(object, ...)
    standardGeneric("predict")
    )

#' Predict Maxent
#'
#' Predict output for a new dataset from a trained \link{Maxent} model.
#'
#' @param object \link{Maxent} object.
#' @param data data.frame containing values used for the prediction.
#' @param type character MaxEnt output type, possible values are "cloglog",
#' "logistic" and "raw", default is "cloglog".
#' @param clamp logical for clumping during prediction, default is TRUE.
#'
#' @details The function performs the prediction in **R** without calling the
#' **MaxEnt** Java software. This results in a faster computation for large
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

    # Clamp observations
    if (clamp) {
      cont_vars <- as.character(object@min_max$variable)
      data[, cont_vars] <- scaleClamp(as.matrix(data[, cont_vars]),
                                      object@min_max$min,
                                      object@min_max$max,
                                      do_clamp = clamp, scale = FALSE)
    }

    f <- object@formula
    # Make the design matrix
    dm <- model.matrix(f, data)
    # Scale features and clamp if clamp is TRUE
    cols <- !grepl("categorical.*|hinge.*|threshold.*", colnames(dm))
    dm[, cols] <- scaleClamp(dm[, cols, drop = FALSE], object@coeff$min[cols],
                             object@coeff$max[cols],
                             do_clamp = clamp, scale = TRUE)

    S <- (dm %*% object@coeff$lambda) - object@lpn
    raw <- exp(S) / object@dn
    raw[raw == Inf | raw > 1] <- 1

    if (type == "raw") {
      return(raw)
    } else if (type == "logistic") {
       return(raw * exp(object@entropy) / (1 + raw * exp(object@entropy)))
    } else {
      return(1 - exp(-raw * exp(object@entropy)))
    }
})
